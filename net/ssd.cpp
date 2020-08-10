#include <iostream>
#include <string>
#include <tuple>
#include <vector>

#include <torch/script.h>
#include <opencv2/opencv.hpp>


struct Frame {
    int label;
    float prob;
    float pos[4];

    Frame(int label, float prob, float w0, float h0, float w1, float h1):
        label(label), prob(prob), pos{w0, h0, w1, h1} { }

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
    float _threshold = 0.80;
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
        if (_getOverlapArea(_framesPtr->at(0), frame) < _threshold)
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


class Net {
  private:
    torch::jit::script::Module _module;

    cv::Size _inp_size = cv::Size(300, 300);
    torch::Tensor _means = torch::full({3}, 127.0);
    float _stdev = 128.0;

    auto _cvprepare(cv::Mat* parsed, const cv::Mat& cv_img) {
        cv::cvtColor(cv_img, *parsed, cv::COLOR_BGR2RGB);
        cv::resize(*parsed, *parsed, _inp_size);
        parsed->convertTo(*parsed, CV_32F, 1.0);
        auto img_tensor = torch::from_blob(parsed->data, {1, 300, 300, 3});
        img_tensor -= _means;
        img_tensor /= _stdev;
        img_tensor.transpose_(1, 3);
        img_tensor.transpose_(2, 3);
        return img_tensor;
    }

    size_t _filter_results(torch::Tensor* preds, torch::Tensor* frames,
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

    auto _parse_frames(const torch::Tensor& preds, const torch::Tensor& frames) {
        std::vector<FrameClass> objects;
        if(preds.sizes()[0] > 0) {
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

  public:
    Net(std::string mod_path) {
        _module = torch::jit::load(mod_path);
    }


    std::vector<Frame> operator()(const cv::Mat& cv_img) {
        cv::Size orginal_size = cv_img.size();
        cv::Mat parsed_img;
        torch::Tensor img_tensor = _cvprepare(&parsed_img, cv_img);

        std::vector<torch::jit::IValue> inputs;
        inputs.emplace_back(img_tensor);
        torch::jit::IValue ans = _module.forward(inputs);
        torch::ivalue::Tuple tuple = *(ans.toTuple().get());
        torch::Tensor preds = tuple.elements().at(0).toTensor()[0];
        torch::Tensor frames = tuple.elements().at(1).toTensor()[0];

        size_t n_results = _filter_results(&preds, &frames);
        if(n_results > 0) {
            frames.index({"...", 0}) *= orginal_size.width;
            frames.index({"...", 1}) *= orginal_size.height;
            frames.index({"...", 2}) *= orginal_size.width;
            frames.index({"...", 3}) *= orginal_size.height;
        }
        auto result = _parse_frames(preds, frames);
        return result;
    }
};


int main(int argc, const char** argv) {
    if (argc != 3) {
        std::cerr << "Please enter the traced model path and image path" << std::endl;
        return -1;
    }
    const char* model_path = argv[1];
    const char* img_path = argv[2];

    Net net(model_path);
    cv::Mat image = cv::imread(img_path, cv::IMREAD_COLOR);
    auto frames = net(image);

    for (auto& frame : frames)
        std::cout << frame << std::endl;

    return 0;
}
