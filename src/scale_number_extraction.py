import cv2 #conda install -c conda-forge opencv
import numpy as np
import pytesseract #conda install -c conda-forge pytesseract

# Load the img
img = cv2.imread("mages/NdHYc.jpg")

# Cvt to hsv
hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)

# Get binary-mask
msk = cv2.inRange(hsv, np.array([0, 0, 175]), np.array([179, 255, 255]))
krn = cv2.getStructuringElement(cv2.MORPH_RECT, (5, 3))
dlt = cv2.dilate(msk, krn, iterations=1)
thr = 255 - cv2.bitwise_and(dlt, msk)

# OCR
d = pytesseract.image_to_string(thr, config="--psm 10")