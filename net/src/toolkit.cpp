#include <algorithm>
#include <list>
#include <mutex>
#include <stdexcept>
#include <string>
#include <vector>
#include <memory>
#include <ostream>
#include "Frame.h"
#include "./toolkit.h"


std::ostream& operator<<(std::ostream& os, Frame f) {
    os << f.label << " " << f.prob << " :\t"
       << f.pos[0] << '\t'<<  f.pos[1] << '\t'
       << f.pos[2] << '\t' << f.pos[3];
    return os;
}


float FrameClass::_getOverlapArea(Frame& f1, Frame& f2) {
    Frame overlap(0, 0, std::max(f1.pos[0], f2.pos[0]),
                        std::max(f1.pos[1], f2.pos[1]),
                        std::min(f1.pos[2], f2.pos[2]),
                        std::min(f1.pos[3], f2.pos[3]));
    return overlap.getArea() / (f1.getArea() + f2.getArea() - overlap.getArea());
}


FrameClass FrameClass::add(Frame frame) {
    if (_framesPtr->at(0).label != frame.label)
        return FrameClass(frame);
    if (_getOverlapArea(_framesPtr->at(0), frame) < _threshold)
        return FrameClass(frame);
    if (_framesPtr->at(0).prob < frame.prob)
        std::swap((*_framesPtr)[0], frame);
    _framesPtr->push_back(frame);
    return *this;
}



NetRunner& NetRunner::getNetRunner(const char* model_path) {
    static NetRunner netRunner;
    if (netRunner._net == nullptr)
        netRunner._net = new Net(model_path);
    return netRunner;
}


std::string parseImgPath(std::string str) {
    auto lastIdx = str.find_last_of(".");
    lastIdx = lastIdx != std::string::npos ? lastIdx : str.length();
    str = str.substr(0, lastIdx);
    lastIdx = str.find_last_of("/");
    lastIdx = lastIdx != std::string::npos ? lastIdx + 1 : 0;
    str = str.substr(lastIdx, str.length() - lastIdx);
    return str;
}


int32_t NetRunner::pushImg(const char* img_path) {
    std::string imname = parseImgPath(img_path);
    auto&& img = cv::imread(img_path, cv::IMREAD_COLOR);
    std::unique_lock<std::mutex> lock(_mtx);
    std::remove(img_path);
    _imgs.push_back(img);
    _imnames.push_back(imname);
    return _imgs.size();
}


uint64_t parseTS(std::string str) {
    auto firstIdx = str.find_first_of("_") + 1;
    str = str.substr(firstIdx, str.length() - firstIdx);
    str.erase(std::remove(str.begin(), str.end(), ':'), str.end());
    str.erase(std::remove(str.begin(), str.end(), '.'), str.end());
    str.erase(std::remove(str.begin(), str.end(), '-'), str.end());
    return std::stoull(str);
}


int32_t NetRunner::_parseInpImgs() {
    int32_t n_imgs = _imgs.size();
    ssize_t bestLeftIdx = -1;
    uint64_t bestLeftTS = 0;
    ssize_t bestRightIdx = -1;
    uint64_t bestRightTS = 0;
    for (int i = 0; i < n_imgs; i++) {
        auto ts = parseTS(_imnames[i]);
        if (_imnames[i][0] == 'l') {
            if (ts > bestLeftTS) {
                bestLeftTS = ts;
                bestLeftIdx = i;
            }
        } else if (_imnames[i][0] == 'r') {
            if (ts > bestRightTS) {
                bestRightTS = ts;
                bestRightIdx = i;
            }
        } else {
            throw std::invalid_argument("image name error");
        }
    }
    if (bestLeftIdx != -1) {
        std::swap(_imgs[0], _imgs[bestLeftIdx]);
        std::swap(_imnames[0], _imnames[bestLeftIdx]);
        if (bestRightIdx == 0)
            bestRightIdx = bestLeftIdx;
    }
    if (bestRightIdx != -1 && n_imgs >= 2) {
        size_t idxToSwap = n_imgs >= 2 ? 1 : 0;
        std::swap(_imgs[idxToSwap], _imgs[bestRightIdx]);
        std::swap(_imnames[idxToSwap], _imnames[bestRightIdx]);
    }
    n_imgs = (bestLeftIdx != -1) + (bestRightIdx != -1);
    _imgs.resize(n_imgs);
    _imnames.resize(n_imgs);
    return n_imgs;
}


int32_t NetRunner::runNet(Frame* frames) {
    std::unique_lock<std::mutex> lock(_mtx);
    int32_t n_imgs = _parseInpImgs();
    if (n_imgs < 2) {
        for (int i = 0; i < n_imgs; i++)
            frames[i] = Frame(0,0,0,0,0,0);
        return n_imgs;
    }
    auto vframes = (*_net)(_imgs);
    for (int i = 0; i < n_imgs; i++) {
        auto bestFrame = *std::max_element(vframes[i].begin(),
                                           vframes[i].end(),
            [](const Frame& f1, const Frame& f2) {
                return (f1.label == 3)*f1.prob < (f2.label == 3)*f2.prob;
            });
        frames[i] = bestFrame.label == 3 ? bestFrame : Frame(0,0,0,0,0,0);
    }
    _imgs.clear();
    return n_imgs;
}


