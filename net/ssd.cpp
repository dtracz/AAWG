#include "ATen/Context.h"
#include "ATen/Functions.h"
#include "ATen/TensorIndexing.h"
#include "ATen/core/TensorBody.h"
#include "ATen/core/ivalue_inl.h"
#include "ATen/core/stack.h"
#include "ATen/record_function.h"
#include "c10/util/Logging.h"
#include "torch/csrc/api/include/torch/types.h"
#include "torch/csrc/autograd/generated/variable_factories.h"
#include "torch/csrc/jit/api/module.h"
#include <mutex>
#include <opencv2/core/core_c.h>
#include <opencv2/core/types.hpp>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include <ostream>
#include <torch/script.h>
#include <memory>
#include <iostream>
#include <string>
#include <tuple>
#include <unordered_set>
#include <utility>
#include <valarray>
#include <vector>
#include <opencv2/opencv.hpp>

struct Frame {
    int label;
    float prob;
    float pos[4];

    Frame(int label, float prob, float w0, float h0, float w1, float h1):
        label(label), prob(prob), pos{w0, h0, w1, h1} { }
    Frame(int label, float prob, float pos[4]): label(label), prob(prob) {
        for (size_t i = 0; i < 4; i++)
            this->pos[i] = pos[i];
    }

    float getArea() {
        return (pos[2] - pos[0]) * (pos[3] - pos[1]);
    }
};

std::ostream& operator<<(std::ostream& os, Frame f) {
    os << f.label << " " << f.prob << " :\t"
       << f.pos[0] << '\t'<<  f.pos[1] << '\t'
       << f.pos[2] << '\t' << f.pos[3];
    return os;
}

class FrameClass {
  private:
    float threshold = 0.80;
    std::shared_ptr<std::vector<Frame>> _framesPtr;

    float _getOverlapArea(Frame& f1, Frame& f2) {
        Frame overlap(0, 0, std::max(f1.pos[0], f2.pos[0]),
                            std::max(f1.pos[1], f2.pos[1]),
                            std::min(f1.pos[2], f2.pos[2]),
                            std::min(f1.pos[3], f2.pos[3]));
        return overlap.getArea() / (f1.getArea() + f2.getArea() - overlap.getArea());
    }


  public:
    FrameClass() = default;
    FrameClass(Frame frame): _framesPtr(std::make_shared<std::vector<Frame>>()) {
        _framesPtr->push_back(frame);
    }

    FrameClass add(Frame frame) {
        if (_framesPtr->at(0).label != frame.label)
            return FrameClass(frame);
        if (_getOverlapArea(_framesPtr->at(0), frame) < threshold)
            return FrameClass(frame);
        if (_framesPtr->at(0).prob < frame.prob)
            std::swap((*_framesPtr)[0], frame);
        _framesPtr->push_back(frame);
        return *this;
    }

    Frame get() const {
        return _framesPtr->at(0);
    }

    bool operator==(FrameClass other) const {
        return _framesPtr == other._framesPtr;
    }
    bool operator!=(FrameClass other) const {
        return _framesPtr != other._framesPtr;
    }
};

class ObjFinder {
  private:
    torch::jit::script::Module module;

    cv::Size inp_size;
    torch::Tensor means = torch::full({3}, 127.0);
    float stdev = 128.0;

    size_t filter_results(torch::Tensor* preds, torch::Tensor* frames,
                        float threshold = 0.5) {
        torch::Tensor maxes = preds->argmax(1);
        *preds = preds->index(maxes > 0);
        *frames = frames->index(maxes > 0);
        if (preds->sizes()[0] == 0)
            return 0;
        maxes = preds->max_values(1);
        *preds = preds->index(maxes > threshold);
        *frames = frames->index(maxes > threshold);
        return preds->sizes()[0];
    }


  public:
    ObjFinder(std::string mod_path) {
        module = torch::jit::load(mod_path);
        inp_size = cv::Size(300, 300);
    }


    std::vector<Frame> operator()(cv::Mat& image) {
        cv::Size orginal_size = image.size();
        cv::cvtColor(image, image, cv::COLOR_BGR2RGB);
        cv::resize(image, image, inp_size);
        image.convertTo(image, CV_32F, 1.0);
        auto img_tensor = torch::from_blob(image.data, {1, 300, 300, 3});
        img_tensor-= means;
        img_tensor /= stdev;
        img_tensor.transpose_(1, 3);
        img_tensor.transpose_(2, 3);
        // img_tensor = img_tensor.transpose(2, 3);

        // std::cout << img_tensor.sizes() << std::endl;

        // std::cout << "***" << std::endl;
        // auto img_part = img_tensor.index({0, 0,
        //         torch::indexing::Slice(140, 165),
        //         torch::indexing::Slice(172, 180)
        //         });
        // std::cout << img_part.sizes() << std::endl;
        // std::cout << img_part << std::endl;

        // std::cout << ":: " << img_tensor.sizes() << std::endl;

        std::vector<torch::jit::IValue> inputs;
        inputs.emplace_back(img_tensor);

        torch::jit::IValue ans = module.forward(inputs);

        torch::ivalue::Tuple tuple = *(ans.toTuple().get());
        torch::Tensor preds = tuple.elements().at(0).toTensor()[0];
        torch::Tensor frames = tuple.elements().at(1).toTensor()[0];

        std::vector<FrameClass> objects;
        if(filter_results(&preds, &frames) > 0) {
            frames.index({"...", 0}) *= orginal_size.width;
            frames.index({"...", 1}) *= orginal_size.height;
            frames.index({"...", 2}) *= orginal_size.width;
            frames.index({"...", 3}) *= orginal_size.height;

            auto maxes = preds.max(1);
            auto probs = std::get<0>(maxes);
            auto labels = std::get<1>(maxes);
            Frame frame(*(labels[0].data_ptr<long>()), *(probs[0].data_ptr<float>()),
                        *(frames[0][0].data_ptr<float>()), *(frames[0][1].data_ptr<float>()),
                        *(frames[0][2].data_ptr<float>()), *(frames[0][3].data_ptr<float>()));
            FrameClass fc(frame);
            objects.push_back(fc);
            for (int i = 1; i < frames.sizes()[0]; i++) {
                frame = Frame(*(labels[i].data_ptr<long>()), *(probs[i].data_ptr<float>()),
                              *(frames[i][0].data_ptr<float>()), *(frames[i][1].data_ptr<float>()),
                              *(frames[i][2].data_ptr<float>()), *(frames[i][3].data_ptr<float>()));
                fc = fc.add(frame);
                if (fc != objects.back()) {
                    objects.push_back(fc);
                }
            }
        }
        std::vector<Frame> result;
        for (auto& fc : objects) {
            result.push_back(fc.get());
        }
        return result;
    }


};


int main(int argc, const char* argv[])
{
    if (argc != 3) {
        std::cerr << "Please enter the traced model path and image" << std::endl;
        return -1;
    }
    // std::shared_ptr<torch::jit::script::Module> module;
    // torch::jit::script::Module module = torch::jit::load(argv[1]);
    // try {
    //     // Deserialize the ScriptModule from a file using torch::jit::load().
    //     module = torch::jit::load(argv[1]);
    // } catch (const c10::Error& e) {
    //     std::cerr << "error loading the model\n";
    //     return -1;
    // }
    // std::cerr << "loaded" << std::endl;

    const char* img_path = argv[2];
    cv::Mat image = cv::imread(img_path, cv::IMREAD_COLOR);


    ObjFinder finder(argv[1]);
    auto frames = finder(image);

    for (auto& frame : frames)
        std::cout << frame << std::endl;

    // // cv::resize(image, image, cv::Size(224, 224));
    // image.convertTo(image, CV_32F, 2.0/255);
    // auto img_tensor = torch::from_blob(image.data, {1, 28, 28});
    // img_tensor -= 1;
    // // std::cout << img_tensor << std::endl;
    //
    // std::vector<torch::jit::IValue> inputs;
    // inputs.emplace_back(img_tensor);
    //
    // auto ans = module.forward(inputs);
    // std::cout << ans.toTensor() << std::endl;



    return 0;
}
