# import cv2 #conda install -c conda-forge opencv
# import numpy as np
# import pytesseract #conda install -c conda-forge pytesseract

# Reads in an image and outputs the number
import cv2
import pytesseract

#pytesseract.pytesseract.tesseract_cmd = r"C:\Program Files\Tesseract-OCR\tesseract.exe"

# Grayscale, Gaussian blur, Otsu's threshold
image = cv2.imread('images/just_number.png')
gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
blur = cv2.GaussianBlur(gray, (3,3), 0)
thresh = cv2.threshold(blur, 0, 255, cv2.THRESH_BINARY_INV + cv2.THRESH_OTSU)[1]

# Morph open to remove noise and invert image
kernel = cv2.getStructuringElement(cv2.MORPH_RECT, (3,3))
opening = cv2.morphologyEx(thresh, cv2.MORPH_OPEN, kernel, iterations=1)
invert = 255 - opening

# Perform text extraction. Not just one block of text (psm=6), psm=7 is better for numbers
data = pytesseract.image_to_string(invert, lang='eng', config='--psm 7')
print(data)

cv2.imshow('thresh', thresh)
cv2.imshow('opening', opening)
cv2.imshow('invert', invert)
cv2.waitKey()

# #Other answer https://stackoverflow.com/questions/51154729/pytesseract-tesseractnotfounderror-python-3

