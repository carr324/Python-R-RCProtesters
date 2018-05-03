# install.packages("rcicr")
# install.packages("jpeg")

# It may be necessary to install the above packages 
# first, if they've never been run before ...

require(rcicr)
require(jpeg)

generateStimuli2IFC(
  base_face_files = list("base" = "bf_bm_wf_wm_blurGrayscaleCropped_400x400.jpg"),
  n_trials = 300, 
  img_size = 400,
  stimulus_path = "./rc_stimuli",
  label = "rcic", 
  use_same_parameters = TRUE,
  seed = 1, 
  maximize_baseimage_contrast = TRUE, 
  noise_type = "sinusoid",
  nscales = 5, 
  sigma = 25
)