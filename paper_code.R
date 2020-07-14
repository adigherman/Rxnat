library(dplyr)
library(fslr)
library(extrantsr)
library(malf.templates)
library(scales)
library(WhiteStripe)

##install.packages('Rxnat')

# Establish XNAT connections
library(Rxnat)
nitrc <- xnat_connect("https://nitrc.org/ir", xnat_name="NITRC")
hcp <- xnat_connect("https://db.humanconnectome.org", xnat_name = "hcp")

# Get list subjects from XNAT
nitrc_subjects <- nitrc$subjects()
hcp_subjects <- hcp$subjects()
all_subjects <- bind_rows(nitrc_subjects, hcp_subjects)
colnames(all_subjects)

# Extract experiment data
nitrc_T1_scan_resources <- nitrc$scans(type="T1")
nitrc_T1_scan_resources <- nitrc_T1_scan_resources %>%
  mutate(source = "nitrc", scantype = "T1")
hcp_T1_scan_resources <- hcp$scans(type="T1w")
hcp_T1_scan_resources <- hcp_T1_scan_resources %>%
  mutate(source = "hcp", scantype = "T1")

# Filter resources and select subjects between 26 and 40 years
T1_resources <- bind_rows(nitrc_T1_scan_resources, hcp_T1_scan_resources)
T1_resources <- left_join(
  T1_resources,
  all_subjects,
  by = c("subject_ID" = "ID")
)
age_26_to_40_group <- T1_resources %>%
  filter(Age>26) %>%
  filter(Age<40)


# Download the first subject T1 weighted image
file_path <- nitrc$download_dir(
  experiment_ID = age_26_to_40_group$experiment_ID[8],
  scan_type = "T1",
  extract = TRUE
)


# Read T1 image
t1 <- readrpi(file_path[1])
ortho2(t1, add.orient = TRUE)

# Remove neck and drop empty dimensions
noneck = remove_neck( file_path,
                      template.file = fslr::mni_fname(brain = TRUE, mm = 1),
                      template.mask = fslr::mni_fname(mm = 1, brain = TRUE, mask = TRUE)
)
red = dropEmptyImageDimensions(noneck)
red <- readrpi(red)


# Inhomegeneity correction
t1_n4 = bias_correct(red,
                     correction = "N4",
                     outfile = tempfile(fileext = ".nii.gz"), retimg = FALSE
)
t1_n4 <- readrpi(t1_n4)

# Perform registration against malf
timgs = mass_images(n_templates = 35)
ss = malf(infile = t1_n4,
          template.images = timgs$images,
          template.structs = timgs$masks,
          keep_images = FALSE)

# Perform skull stripping
proc_outfile <- paste0("T1_Processed.nii.gz")
proc_outfile <- file.path(tempdir(),proc_outfile)
skull_ss <- preprocess_mri_within(
  files = t1_n4,
  outfiles = proc_outfile,
  correction = "N4",
  maskfile = ss,
  correct_after_mask = FALSE)
t1_ss <- readrpi(proc_outfile)

# Perform WhiteStripe intensity normalization
ind = whitestripe(img = t1_ss, type = "T1", stripped = TRUE)$whitestripe.ind
ws_t1 = whitestripe_norm(t1_ss, ind)

# Perform segmentation
ss_tcs = fslr::fast_nobias(ws_t1,
                           verbose = TRUE)

# Generate Panel A
ortho2(red,
       add.orient = TRUE,
       addlegend = TRUE,
       leg.cex = 1.5,
       leg.title = "Panel A",
       leg.col = "red",
       legend = "T1 Image Neck Removed",
       leg.x = 0
)

# Generate Panel B
ortho2(red,
       t1_ss,
       col.y=alpha("red", 0.3),
       add.orient = TRUE,
       addlegend = TRUE,
       leg.cex = 1.5,
       leg.title = "Panel B",
       leg.col = "red",
       legend = "T1 Image Brain Mask",
       leg.x = 0
)

# Generate Panel C
double_ortho(ws_t1,
             ss_tcs,
             add.orient=TRUE,
             addlegend =TRUE,
             leg.cex=1.5,
             leg.title="Panel C",
             leg.col="red",
             legend="Segmentation results",
             leg.x=0
)
