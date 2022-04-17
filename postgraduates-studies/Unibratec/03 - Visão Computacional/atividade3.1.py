#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Apr 13 11:39:35 2019

@author: macair
"""

import cv2
import numpy as np

img_gray = cv2.imread('ponce object recognition dataset/objects/obj02_001.jpg', cv2.IMREAD_GRAYSCALE)
# cv2.imshow("P&B IMAGE", img_gray)

detector = cv2.AKAZE_create(threshold=0.001, nOctaves=4, nOctaveLayers=4)
kps = detector.detect(img_gray)
result_img = cv2.drawKeypoints(img_gray, kps, np.array([]), flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)

cv2.imwrite("cresult.png", result_img)

# cv2.imshow('Result Image', result_img)
# cv2.waitKey(0)