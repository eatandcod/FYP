import base64
import io
import os
from email.mime.image import MIMEImage
import os

os.environ['TF_CPP_MIN_LOG_LEVEL'] = '2'  # Suppress TensorFlow logging (1: INFO, 2: WARNING, 3: ERROR)

import tensorflow as tf

import cv2

import streamlit as st
from tensorflow.keras.models import load_model
from tensorflow.keras.preprocessing import image
import numpy as np
from PIL import Image
import matplotlib.pyplot as plt
import joblib


# Function to load the CNN model
@st.cache_resource
def load_my_model():
    model = load_model('binary_CNN.h5')
    return model


@st.cache_resource
def load_rf_model():
    clf = joblib.load('wildfire_random_forest_model.pkl')
    return clf


def load_svm():
    clf = joblib.load('svm_fire.pkl')
    return clf


# Load your model
model = load_my_model()
rf_model = load_rf_model()
svm_mod = load_svm()


# Function to preprocess and predict the fire in the image
def predict(model, img):
    img = img.resize((64, 64))
    img = image.img_to_array(img)
    img = np.expand_dims(img, axis=0)
    img /= 255.0

    # Use the model to predict
    result = model.predict(img)
    # st.write('Debug: Raw prediction output:', result)  # Debug information
    return 'fire' if result[0][0] < 0.5 else 'no fire'


def predict_rf(model, img):
    img = img.resize((64, 64))
    img = np.array(img)
    img = img.flatten()  # Flatten the image
    img = img.reshape(1, -1)  # Reshape for the model
    result = model.predict(img)
    return 'fire' if result[0] == 1 else 'no fire'


svm_model = joblib.load('svm_fire.pkl')


# Function to predict using SVM
def predict_svm(image):
    image = cv2.resize(image, (64, 64))  # Resize image
    image = image.flatten().reshape(1, -1)  # Flatten and reshape image
    prediction = svm_model.predict(image)  # Predict using the SVM model
    return 'fire' if prediction[0] == 1 else 'no fire'


# Rothermel spread rate calculation function
def calculate_spread_rate(I_R, rho_b, epsilon, Q_ig, phi_W, phi_S):
    heat_source = I_R * (1 + phi_W + phi_S)
    heat_sink = rho_b * epsilon * Q_ig
    R = heat_source / heat_sink
    return R


# Function to plot the graph for Rothermel spread rate
def plot_spread_rate(phi_range, spread_rates, factor_name):
    plt.figure(figsize=(10, 4))
    plt.plot(phi_range, spread_rates, '-o')
    plt.title(f'Fire Spread Rate vs {factor_name}')
    plt.xlabel(f'{factor_name} Factor')
    plt.ylabel('Fire Spread Rate (m/min)')
    plt.grid(True)
    st.pyplot(plt)


# Navigation Sidebar
st.sidebar.title('Navigation')
options = st.sidebar.radio('Select a page:', ['Home', 'Fire Images', 'News', 'Contact', 'Spread Rate'])


def audio_alert():
    # Open audio file
    audio_file = open('sound/alert_sound.mp3', 'rb')
    audio_bytes = audio_file.read()

    # Convert audio bytes to base64
    base64_audio = base64.b64encode(audio_bytes).decode('utf-8')

    # Embed as HTML with autoplay attribute set to true
    audio_html = f"""
    <audio autoplay controls style="display:none;">
      <source src="data:audio/mp3;base64,{base64_audio}" type="audio/mp3">
    Your browser does not support the audio element.
    </audio>
    """
    st.markdown(audio_html, unsafe_allow_html=True)

from streamlit_elements import elements, mui, sync

# Define your app logic




import base64


def get_base64_of_image(image_path):
    with open(image_path, "rb") as img_file:
        return base64.b64encode(img_file.read()).decode('utf-8')


image_base64 = get_base64_of_image("static/img_3.png")

st.markdown(f"""
<style>
.stApp {{
background-image: url("data:image/png;base64,{image_base64}");
background-size: cover !important; 
            background-size: cover !important;
            background-position: center !important;
            background-repeat: no-repeat !important;
            background-attachment: fixed !important;
        
}}  
/* This targets the file uploader button by its role attribute */
div[role="button"][aria-label="Upload"] {{
    background-color: #66BB6A !important;
}}

/* This styles the main title text */
.header-title {{
    font-size: 3em; /* Large font size */
    font-weight: bold; /* Bold font */
    color: #013220; /* Light text color for contrast */
    text-shadow: 0px 4px 4px rgba(0, 0, 0, 0.25); /* Text shadow for depth */
    margin-bottom: 0.5em; /* Space below the title */
}}

/* Apply the custom style to the title */
</style>
""", unsafe_allow_html=True)
st.markdown("""
    <style>
    @import url('https://fonts.googleapis.com/css2?family=Lobster&display=swap'); /* Importing the 'Lobster' font from Google Fonts */

    /* Custom CSS for the title */
    .title {
        
        text-align: center; /* Center the title */
        margin-top: 0.5em; /* Adjust the top margin to position the title */
    } 
    
    </style>
""", unsafe_allow_html=True)

st.markdown("""
    <style>
    /* Custom CSS for moving the text to the left */
    .move-left {
        position: relative;
        right: 50%; /* Adjust the percentage to move the text further to the left */ 
        border: 2px solid red;
    border-radius: 4px;
    padding: 8px;
    box-shadow: 0 0 8px red;
    display: inline-block;
    color: white;
    background-color: black;
    margin: 10px; 
    font-family: 'Lobster', cursive; /* Using the 'Lobster' font */
        font-size: 2em; /* Adjust the size as needed */
        color: #FFA07A; /* Choose a color that stands out on your background */
        text-shadow: 2px 2px 4px #000000; /* Shadow effect for depth */ 
        animation-name: pulseAnimation;
        animation-duration: 2s;
        animation-iteration-count: infinite;
        animation-timing-function: ease-in-out; 
        
    } 
    /* Keyframes for animation */
    @keyframes pulseAnimation {
        0% { transform: scale(1); }
        50% { transform: scale(1.1); }
        100% { transform: scale(1); }
    }

    /* Custom CSS for the animated text */
    .animated-text {
        animation-name: pulseAnimation;
        animation-duration: 2s;
        animation-iteration-count: infinite;
        animation-timing-function: ease-in-out;
    }
    </style>
""", unsafe_allow_html=True)

# Use the custom class for your text
st.markdown('<div class="move-left">Welcome to the Forest Fire Detection Dashboard</div>', unsafe_allow_html=True)



# Home Page with Image Prediction
if options == 'Home':

    # Define custom styles for buttons
    button_style = """
    <style>
    .custom-button {
        background-color: #FFA07A; /* Light shade of orange */
        border: none;
        color: black;
        padding: 10px 24px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 12px; /* Rounded corners */
    }

    .custom-button:hover {
        background-color: #ff7f50; /* Slightly darker shade of orange */
    }

    .button-container {
        text-align: left; /* Align the buttons to the left */
    }
    </style>
    """
    # Apply the custom styles
    st.markdown(button_style, unsafe_allow_html=True)

    # Create button container
    st.markdown('<div class="button-container">', unsafe_allow_html=True)

    # Upload Image Button
    st.markdown(
        '<button class="custom-button" onclick="document.getElementById(\'uploadButton\').click()">Upload Image</button>',
        unsafe_allow_html=True
    )
    uploaded_file = st.file_uploader("", type=['jpg', 'jpeg', 'png'], key="uploadButton", help="Upload your image here")

    # Capture Image Button
    st.markdown(
        '<button class="custom-button" onclick="document.getElementById(\'captureButton\').click()">Capture Image</button>',
        unsafe_allow_html=True
    )
    captured_image = st.camera_input("", key="captureButton", help="Capture your image here")

    st.markdown('</div>', unsafe_allow_html=True)  # Close button container

    # If an image was uploaded or captured, display and predict
    if uploaded_file is not None or captured_image is not None:
        img = Image.open(uploaded_file) if uploaded_file is not None else Image.open(
            io.BytesIO(captured_image.getvalue()))
        st.image(img, caption='Selected Image', use_column_width=True)
        # Prediction logic goes here...


        def show_prediction_result(prediction):
            # Define the CSS styles
            result_style = """
            <style>
                .prediction-box {
                    border: 2px solid #013220; /* Dark green border */
                    background-color: #A5D6A7; /* Light green background */
                    border-radius: 5px; /* Rounded corners */
                    padding: 10px; /* Padding inside the box */
                    margin: 10px 0; /* Margin outside the box */
                    color: #013220; /* Dark green text */
                }
            </style>
            """

            # Use markdown to display the prediction result with the defined style
            st.markdown(result_style, unsafe_allow_html=True)
            st.markdown(f"<div class='prediction-box'>{prediction}</div>", unsafe_allow_html=True)


        # CNN Prediction Button
        if st.button('Predict Image with CNN'):
            with st.spinner('Predicting with CNN...'):
                prediction = predict(model, img)
                show_prediction_result(f'The CNN prediction is: {prediction}')
                if prediction == 'fire':
                    audio_alert()  # Play the alert sound if fire is detected

        # Random Forest Prediction Button
        if st.button('Predict Image with Random Forest'):
            with st.spinner('Predicting with Random Forest...'):
                prediction = predict_rf(rf_model, img)
                show_prediction_result(f'The RF prediction is: {prediction}')
                if prediction == 'fire':
                    audio_alert()  # Play the alert sound if fire is detected

        # SVM Prediction Button
        if st.button('Predict Image with SVM'):
            with st.spinner('Predicting with SVM...'):
                # Initialize variable for the image array
                opencv_image = None

                # Check if an uploaded image is available
                if uploaded_file is not None:
                    img = Image.open(uploaded_file).convert('RGB')
                    opencv_image = np.array(img)  # Convert PIL image to a numpy array

                # Check if an image was captured with the camera
                elif captured_image is not None:
                    # Convert the captured image to a numpy array
                    img = Image.open(io.BytesIO(captured_image.getvalue()))
                    opencv_image = np.array(img)  # Convert PIL image to a numpy array

                # If an image was uploaded or captured, make a prediction
                if opencv_image is not None:
                    opencv_image = cv2.cvtColor(opencv_image, cv2.COLOR_RGB2BGR)  # Convert RGB to BGR
                    prediction = predict_svm(opencv_image)  # Make prediction
                    show_prediction_result(f'The svm prediction is: {prediction}')
                    if prediction == 'fire':
                        audio_alert()

# Spread Rate Page
elif options == 'Spread Rate':
    # Define styles for the container and sidebar
    st.markdown("""
        <style>
        .reportview-container .main .block-container{
            max-width: 700px; /* Limit the maximum width */
            padding-top: 5rem; /* Top padding */
            padding-bottom: 5rem; /* Bottom padding */
        }
        .sidebar .sidebar-content {
            background-color: #f1f3f6; /* Sidebar background color */
        }
        .css-12w0qpk {
            background-color: #f9f9f9; /* Main background color */
            border: 1px solid #e1e4e8; /* Border color */
        }   
        .stForm {
            background-color: #FFCC99 !important; /* Light orange background color */
            padding: 20px !important; /* Padding inside the form */
            border-radius: 10px !important; /* Rounded corners for the form */
            box-shadow: 0px 0px 10px rgba(0,0,0,0.1) !important; /* Subtle shadow around the form */
        }
        /* Style for form widgets */
        .stForm .stTextInput, .stForm .stSelectbox, .stForm .stSlider, .stForm .stNumberInput {
            background-color: #FFCC99 !important; /* Background color for form widgets */
        }
        .dashboard-title {
            background-color: black; /* Background color for the title */
            color: white; /* Text color for the title */
            padding: 10px; /* Padding around the text */
            border-radius: 10px; /* Rounded corners for the background */
            display: inline-block; /* Ensures the background only spans the text */
        }  
        .stForm {
    background-color: #FFFF00 !important; /* Light orange background color */
    padding: 20px !important; /* Padding inside the form */
    border-radius: 10px !important; /* Rounded corners for the form */
    box-shadow: 0px 0px 10px rgba(0,0,0,0.1) !important; /* Subtle shadow around the form */
} 
.stForm input[type="number"], .stForm input[type="range"] {
        background-color: #FFA500; /* Orange background color */
        color: black; /* Black text color */
    }

        </style>
        """, unsafe_allow_html=True)


    # Set up the layout with columns
    col1, _ = st.columns([3, 1])  # Adjust the ratio as needed

    with col1:
        st.markdown("<h2 class='dashboard-title'>Spread Rate Calculation</h2>", unsafe_allow_html=True)

        # Use a form for the input parameters
        with st.form("spread_rate_form"):
            I_R = st.number_input("Reaction Intensity (I_R) in kW/m^2", value=8000.0, format="%.2f")
            rho_b = st.number_input("Bulk Density (rho_b) in kg/m^3", value=32.0, format="%.2f")
            epsilon = st.number_input("Effective Heating Number (epsilon)", value=0.01, format="%.4f")
            Q_ig = st.number_input("Heat of Preignition (Q_ig) in kJ/kg", value=2500.0, format="%.2f")
            phi_W = st.slider("Wind Factor (phi_W)", min_value=0.0, max_value=1.0, value=0.4)
            phi_S = st.slider("Slope Factor (phi_S)", min_value=0.0, max_value=1.0, value=0.2)

            # Submit button for the form
            submitted = st.form_submit_button("Calculate Spread Rate")
            if submitted:
                spread_rate = calculate_spread_rate(I_R, rho_b, epsilon, Q_ig, phi_W, phi_S)
                st.success(f"The predicted fire spread rate is {spread_rate:.2f} m/min")

    # Placeholders for additional content or spacing can be added here if needed

    # If the form is submitted, perform the calculations
    if submitted:
        wind_factors = np.linspace(0, 1, 100)
        slope_factors = np.linspace(0, 1, 100)
        spread_rates_wind = [calculate_spread_rate(I_R, rho_b, epsilon, Q_ig, phi, phi_S) for phi in wind_factors]
        spread_rates_slope = [calculate_spread_rate(I_R, rho_b, epsilon, Q_ig, phi_W, phi) for phi in slope_factors]

        # Call functions to plot spread rate against wind and slope factors
        plot_spread_rate(wind_factors, spread_rates_wind, "Wind")
        plot_spread_rate(slope_factors, spread_rates_slope, "Slope")




# Other pages...
elif options == 'Fire Images':
    st.write('Gallery of fire images.')
    # Display fire images here
elif options == 'News':
    st.write('Latest news on forest fires.')
    # Show latest news on forest fires
elif options == 'Contact':
    st.write('Contact us at info@example.com.')
    # Show contact information or form

# Remember to handle the 'Fire Images', 'News', 'Contact' pages as needed
