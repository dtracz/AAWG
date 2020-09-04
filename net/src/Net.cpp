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



torch::Tensor Net::_cvprepare(std::vector<cv::Mat>& cv_imgs) {
    std::vector<torch::Tensor> img_tensors;
    for (auto& cv_img : cv_imgs) {
        cv::cvtColor(cv_img, cv_img, cv::COLOR_BGR2RGB);
        cv::resize(cv_img, cv_img, _inp_size);
        cv_img.convertTo(cv_img, CV_32F, 1.0);
        auto&& img_tensor = torch::from_blob(cv_img.data, {1, 300, 300, 3});
        img_tensors.push_back(img_tensor);
    }
    auto imgs_tensor = torch::cat(img_tensors, 0);
    imgs_tensor -= _means;
    imgs_tensor /= _stdev;
    imgs_tensor.transpose_(1, 3);
    imgs_tensor.transpose_(2, 3);
    return imgs_tensor;
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


std::vector<std::vector<Frame>> Net::operator()(std::vector<cv::Mat>& cv_imgs) {
    std::vector<cv::Size> orginal_sizes;
    for (auto& cv_img : cv_imgs)
        orginal_sizes.push_back(cv_img.size());
    auto imgs_tensor =_cvprepare(cv_imgs); 
   
    std::vector<torch::jit::IValue> inputs;
    inputs.emplace_back(imgs_tensor);
    torch::jit::IValue ans = _module.forward(inputs);
    torch::ivalue::Tuple tuple = *(ans.toTuple().get());

    std::vector<std::vector<Frame>> results;
    for (size_t i = 0; i < orginal_sizes.size(); i++) {
        torch::Tensor preds = tuple.elements().at(0).toTensor()[i];
        torch::Tensor frames = tuple.elements().at(1).toTensor()[i];
        
        /*!
         * it's best to have frames in range [0,1]
         */
        // size_t n_results = _filter_results(&preds, &frames);
        // if(n_results > 0) {
        //     frames.index({"...", 0}) *= orginal_sizes[i].width;
        //     frames.index({"...", 1}) *= orginal_sizes[i].height;
        //     frames.index({"...", 2}) *= orginal_sizes[i].width;
        //     frames.index({"...", 3}) *= orginal_sizes[i].height;
        // }
        auto result = _parse_frames(preds, frames);
        results.push_back(result);
    }
    return results;
}


