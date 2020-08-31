#include <iostream>
#include <string>
#include <tuple>
#include <vector>
#include <opencv2/imgcodecs.hpp>
#include <opencv2/imgproc.hpp>
#include "ATen/core/TensorBody.h"
#include "torch/csrc/api/include/torch/types.h"
#include "torch/csrc/autograd/generated/variable_factories.h"
#include "torch/csrc/jit/api/module.h"
#include "torch/csrc/jit/serialization/import.h"
#include "Frame.h"
#include "Net.h"
#include "./toolkit.h"



torch::Tensor Net::_cvprepare(cv::Mat* parsed, const cv::Mat& cv_img) {
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


size_t Net::_filter_results(torch::Tensor* preds, torch::Tensor* frames,
                    float threshold) {
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


std::vector<Frame> Net::_parse_frames(const torch::Tensor& preds,
                                      const torch::Tensor& frames) {
    std::vector<FrameClass> objects;
    if(preds.sizes()[0] > 0) {
        auto maxes = preds.max(1);
        auto probs = std::get<0>(maxes);
        auto labels = std::get<1>(maxes);
        Frame frame(*(labels[0].data_ptr<long>()),
                    *(probs[0].data_ptr<float>()),
                    *(frames[0][0].data_ptr<float>()),
                    *(frames[0][1].data_ptr<float>()),
                    *(frames[0][2].data_ptr<float>()),
                    *(frames[0][3].data_ptr<float>()));
        FrameClass fc(frame);
        objects.push_back(fc);
        for (int i = 1; i < frames.sizes()[0]; i++) {
            frame = Frame(*(labels[i].data_ptr<long>()),
                          *(probs[i].data_ptr<float>()),
                          *(frames[i][0].data_ptr<float>()),
                          *(frames[i][1].data_ptr<float>()),
                          *(frames[i][2].data_ptr<float>()),
                          *(frames[i][3].data_ptr<float>()));
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


std::vector<Frame> Net::operator()(const cv::Mat& cv_img) {
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


