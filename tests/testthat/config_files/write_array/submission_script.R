# # Try to write two components with the same name
#
# library(rFDP)
#
# config_file <- "../FDP_validation/write_array/config.yaml"
#
# fdp_pull(config_file)
# fdp_run(config_file, skip = TRUE)
#
# # Open the connection to the local registry with a given config file
# h <- initialise()
#
# # Return location of file stored in the pipeline
# input_path <- link_read(h, "management-data")
#
# process_cam_ambulance(h, input_path)
# process_cam_calls(h, input_path)
# process_cam_carehomes(h, input_path)
# process_cam_hospital(h, input_path)
# process_cam_mortality(h, input_path)
# process_cam_nhsworkforce(h, input_path)
# process_cam_schools(h, input_path)
# process_cam_testing(h, input_path)
#
# finalise(h)
