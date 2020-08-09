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
#include <torch/script.h>
#include <memory>
#include <iostream>
#include <string>
#include <utility>
#include <vector>
#include <opencv2/opencv.hpp>



class ObjFinder {
  private:
    torch::jit::script::Module module;

    cv::Size inp_size;
    torch::Tensor means = torch::full({3}, 127.0);
    float stdev = 128.0;

    size_t filter_results(torch::Tensor* preds, torch::Tensor* frames,
                        float threshold = 0.5) {
        // torch::indexing::
        torch::Tensor maxs = preds->argmax(1);
        // std::cout << maxs << std::endl;
        // std::cout << preds->index(maxs > 0) << std::endl;
        *preds = preds->index(maxs > 0);
        *frames = frames->index(maxs > 0);
        if (preds->sizes()[0] == 0)
            return 0;
        maxs = preds->max_values(1);
        *preds = preds->index(maxs > threshold);
        *frames = frames->index(maxs > threshold);
        return preds->sizes()[0];
    }


  public:
    ObjFinder(std::string mod_path) {
        module = torch::jit::load(mod_path);
        inp_size = cv::Size(300, 300);
    }


    auto operator()(cv::Mat& image) {
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

        std::cout << img_tensor.sizes() << std::endl;

        // std::cout << "***" << std::endl;
        auto img_part = img_tensor.index({0, 0,
                torch::indexing::Slice(140, 165),
                torch::indexing::Slice(172, 180)
                });
        std::cout << img_part.sizes() << std::endl;
        std::cout << img_part << std::endl;

        // std::cout << ":: " << img_tensor.sizes() << std::endl;

        std::vector<torch::jit::IValue> inputs;
        inputs.emplace_back(img_tensor);

        torch::jit::IValue ans = module.forward(inputs);

        torch::ivalue::Tuple tuple = *(ans.toTuple().get());
        torch::Tensor preds = tuple.elements().at(0).toTensor()[0];
        torch::Tensor frames = tuple.elements().at(1).toTensor()[0];

        if(filter_results(&preds, &frames) > 0) {
            frames.index({"...", 0}) *= orginal_size.width;
            frames.index({"...", 1}) *= orginal_size.height;
            frames.index({"...", 2}) *= orginal_size.width;
            frames.index({"...", 3}) *= orginal_size.height;
        }

        std::cout << "____________________________________________________" << std::endl;
        std::cout << preds.argmax(1) << std::endl;
        std::cout << frames << std::endl;

        return ans;
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
    auto ans = finder(image);

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
