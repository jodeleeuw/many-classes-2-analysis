## ----------------------------------
## GET_MODERATORS.R
## ----------------------------------
# This script fetches a variety
# of data at different levels,
# and organizes it to enable
# moderator analyses.

# Clean-up
rm(list=ls())

# Required Libraries
require(tidyverse)
require(readxl)

# Load the processed data
load(url("https://osf.io/mzf6h/download"))
# load(file = "processed_data.Rdata")

# Load the analysis set
load(url("https://osf.io/yks7a/download"))
# load(file = "analysis_set.Rdata")

# Load prequestions codes
tempfile <- tempfile(fileext = ".xlsx")
download.file("https://osf.io/fbngm/download/", tempfile, mode = "wb")
prequestion_codes <- read_excel(tempfile)
# load(file = "prequestion_codes.Rdata")

# Load video codes
tempfile <- tempfile(fileext = ".xlsx")
download.file("https://osf.io/6w5hy/download/", tempfile, mode = "wb")
video_codes <- read_excel(tempfile)
# load(file = "video_codes.Rdata")

#
# CLASS-LEVEL DATA: mod_class
#  course_id: identifier for each class
#  nbr_other_assessments: number of graded assessments in the course
#
mod_class <- classes %>% select(course_id,nbr_other_assessments)

#
# EXAM-LEVEL DATA: mod_exam
# Note: Here, "exam" is a combination of course and exposure
#  course_id: identifier for each class
#  exposure: treatment number (values 1 or 2)
#  pct_correct: overall percent correct on the exam items
#  delay: days between prequestion due date and exam date
#
mod_exam <- classes %>% mutate(exposure1_due_date = as.POSIXct(exposure1_due_date),
                               exposure2_due_date = as.POSIXct(exposure2_due_date),
                               outcome_assessment_date = as.POSIXct(outcome_assessment_date)) %>% 
                        mutate(exposure1_delay = difftime(outcome_assessment_date,exposure1_due_date, units="days"),
                               exposure2_delay = difftime(outcome_assessment_date,exposure2_due_date, units="days")) %>%
                        select(course_id,exposure1_delay,exposure2_delay) %>% 
                        pivot_longer(cols=c(exposure1_delay,exposure2_delay),names_pattern = "(.*)_delay") %>% 
                        select(course_id,exposure = name, delay = value) %>% 
                        mutate(exposure = if_else(exposure == "exposure1",1,2),
                               delay = round(as.numeric(delay)))
exam_difficulty <- outcomes %>% group_by(course_id, outcome_name) %>% 
                                drop_na() %>%
                                summarize(pct_correct = mean(outcome_score/points_possible)) %>%
                                filter(outcome_name %in% c("Exposure 1","Exposure 2")) %>% 
                                select(course_id, exposure = outcome_name, pct_correct) %>% 
                                mutate(exposure = if_else(exposure == "Exposure 1",1,2))
mod_exam <- merge(mod_exam,exam_difficulty,by=c("course_id","exposure"),all.x=TRUE)
rm(exam_difficulty)

#
# STUDENT-LEVEL DATA: mod_student
#  participant_id: identifier for each student
#  course_id: identifier for each class
#  level: student's academic level
#  pretest: student's grade standing prior to experiment
#  in_major: whether the course is within the student's academic program
#  prequestion_score: percent correct on prequestions
#
mod_student <- datFrm %>% select(participant_id,course_id,level,pretest,in_major,prequestion_score)

# 
# SUBMISSION-LEVEL DATA: mod_submission
# Note: Here, "submission" is a combination of student and exposure
#  participant_id: identifier for each student
#  course_id: identifier for each class
#  exposure: treatment number (values 1 or 2)
#  condition: name of treatment condition
#  viewpct: Percent of video watched by the student
#  initiate_playback: Flag for whether viewpct > 0.
#  nevents: Number of media playback events (clicks) by the student
#  duration: Cumulative time spent on assignment
#  submDaysBeforeDueDate: Number of days before the due date that the assignment was first completed
# 
mod_submission <- datFrm %>% select(participant_id,course_id,exposure_order,
                                                             viewpct_prequestions,viewpct_control,
                                                             nevents_prequestions,nevents_control,
                                                             duration_prequestions,duration_control,
                                                             submDaysBeforeDueDate_prequestions,submDaysBeforeDueDate_control) %>%
                             pivot_longer(cols = -c(participant_id,course_id,exposure_order),
                                          names_to = c(".value", "condition"), 
                                          names_pattern = "(.*)_(.*)") %>%
                             mutate(exposure = case_when(
                               exposure_order == "control_then_prequestion"  & condition == "control" ~ 1,
                               exposure_order == "control_then_prequestion"  & condition == "prequestions" ~ 2,
                               exposure_order == "prequestions_then_control" & condition == "control" ~ 2,
                               exposure_order == "prequestions_then_control" & condition == "prequestions" ~ 1,
                               .default = NA)) %>%
                             mutate(initiate_playback = if_else(viewpct > 0,1,0)) %>%
                             select(participant_id, course_id, exposure, condition, viewpct, initiate_playback,
                                    nevents,duration, submDaysBeforeDueDate)

# 
# ASSIGNMENT-LEVEL DATA: mod_assignment
#  course_id: identifier for each class
#  exposure: treatment number (values 1 or 2)
#  video_id: YouTube video ID
#  video_quality: overall rating of the quality of the video by an undergraduate RA
#  nimages: number of images shown in the video
#  nslides: number of slides shown in the video
#  nwords: number of words in automatic video transcript
#  video_length: total duration of video
#  video_monitor: was there a video monitor of the instructor in the video?
#  avg_time_of_prequestions: avg percent of the video elapsed when prequestions are addressed
#  time_addressing_preqs: cumulative time spent addressing prequestions in video
#  inst_read_text: qualitative rating of how much the instructor read the text verbatim
#  answer_not_provided: was there at least one prequestion where the answer was never presented (learner needed to infer answer)
#  require_memorization: Does any prequestion involve memorization of a word/term/phrase?
#  preq_difficulty: percent correct (overall) on the prequestions associated with the video
#  
videos_by_courses <- events %>% select(course_id,video_id) %>% unique() %>% drop_na() %>%
                                mutate(video_id = if_else(video_id=="rxy1Wu4kxgw","xy1Wu4kxgw",video_id)) %>%
                                mutate(video_id = if_else(video_id=="_z_hji8doCQ","z_hji8doCQ",video_id)) # fix typos in video_id
mod_assignment <- video_codes %>% select(video_id,
                                         video_quality = how_does_video_compare_with_others,
                                         nimages = number_of_images,
                                         nslides = number_of_slides,
                                         nwords = words,
                                         video_length = duration,
                                         video_monitor = use_of_video_monitor)
mod_assignment <- merge(videos_by_courses,mod_assignment,by="video_id",all.x=TRUE)
rm(videos_by_courses)
mod_assignment2 <- prequestion_codes %>% mutate(across(c(time_answer_begins), ~ na_if(., 0))) %>%
                                         mutate(no_answer_presentation = if_else(no_answer_presentation == "yes",1,0)) %>%
                                         mutate(does_question_involve_memorization = if_else(does_question_involve_memorization == "yes",1,0)) %>%
                                         group_by(video_id) %>%
                                         summarize(exposure = mean(video_index),
                                                   avg_time_of_prequestions = mean(time_answer_begins,na.rm = TRUE),
                                                   time_answering_preqs = sum(time_answer_duration),
                                                   inst_read_text = mean(rating_readingslides,na.rm = TRUE),
                                                   answer_not_provided = max(no_answer_presentation),
                                                   require_memorization = max(does_question_involve_memorization))
mod_assignment <- merge(mod_assignment,mod_assignment2,by=c("video_id"),all.x = TRUE)
rm(mod_assignment2)
mod_assignment3 <- datFrm %>% mutate(exposure = if_else(exposure_order == "control_then_prequestion",2,1)) %>% 
                              group_by(course_id,exposure) %>%
                              summarize(preq_difficulty = mean(prequestion_score,na.rm = TRUE))
mod_assignment <- merge(mod_assignment,mod_assignment3,by=c("course_id","exposure"))
rm(mod_assignment3)

# You know what? -- mod_exam and mod_assignment are at the same level: the level
# of the exposure, and these should really be in the same data frame. So let's 
# combine them into:
#
# EXPOSURE-LEVEL DATA: mod_exposure
# Note: This is a combination of mod_exam and mod_assignment, above
mod_exposure <- merge(mod_exam,mod_assignment,by=c("course_id","exposure"),all.x=TRUE)

# Remove everything but the moderator data frames we created
rm(list=ls()[! ls() %in% c("mod_class","mod_exposure","mod_student","mod_submission")])

save(mod_class, mod_exposure, mod_student, mod_submission, file="moderators.Rdata")