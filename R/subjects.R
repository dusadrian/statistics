`subjects` <- function(x) {
    UseMethod ("subjects")
}

`subjects.default` <- function(x) {
    data <- structure(list(dataset = c("civic_attitudes.rds", "community_health.rds",
    "media_use.rds", "students.rds", "workplace.rds", "media_use.rds",
    "students.rds", "workplace.rds", "workplace.rds", "workplace.rds",
    "civic_attitudes.rds", "community_health.rds", "media_use.rds",
    "students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
    "media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
    "community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
    "community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
    "community_health.rds", "community_health.rds", "media_use.rds",
    "students.rds", "workplace.rds", "community_health.rds", "media_use.rds",
    "students.rds", "workplace.rds", "community_health.rds", "media_use.rds",
    "students.rds", "community_health.rds", "students.rds", "civic_attitudes.rds",
    "community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
    "civic_attitudes.rds", "community_health.rds", "media_use.rds",
    "students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
    "media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
    "community_health.rds", "civic_attitudes.rds", "community_health.rds",
    "media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
    "community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
    "civic_attitudes.rds", "community_health.rds", "media_use.rds",
    "students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
    "students.rds", "civic_attitudes.rds", "community_health.rds",
    "media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
    "community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
    "civic_attitudes.rds", "community_health.rds"), iv_name = c("volunteer",
    "access_clinic", "gender", "scholarship", "contract_type", "fact_check",
    "first_gen", "union_member", "contract_type", "union_member",
    "volunteer", "access_clinic", "gender", "scholarship", "union_member",
    "volunteer", "smoking", "gender", "gender", "contract_type",
    "volunteer", "smoking", "fact_check", "scholarship", "union_member",
    "access_clinic", "gender", "first_gen", "contract_type", "smoking",
    "health_status", "platform_primary", "gender", "sector", "bmi_category",
    "news_trust_group", "study_hours_level", "sector", "access_clinic",
    "platform_primary", "first_gen", "exercise_level", "specialization",
    "vote_intent", "bmi_category", "screen_time_group", "study_hours_level",
    "union_member", "residency", "neighborhood", "gender", "gender",
    "union_member", "vote_intent", "access_clinic", "gender", "specialization",
    "contract_type", "education", "neighborhood", "residency", "bmi_category",
    "news_trust_group", "specialization", "burnout_level", "vote_intent",
    "exercise_level", "screen_time_group", "attendance_band", "sector",
    "trust_group", "health_status", "platform_primary", "grades_band",
    "sector", "age_group", "age_group", "specialization", "education",
    "age_group", "screen_time_group", "attendance_band", "weekly_hours_group",
    "age_group", "bmi_category", "news_trust_group", "specialization",
    "sector", "education", "bmi_category"), dv_name = c("civic_engagement",
    "exercise_days", "political_interest", "study_hours", "training_hours",
    "news_trust", "stress_score", "burnout_score", "job_satisfaction",
    "training_hours", "policy_support", "age", "age", "attendance_pct",
    "job_satisfaction", "trust_gov", "age", "screen_hours", "stress_score",
    "remote_days", "age", "bmi", "political_interest", "stress_score",
    "weekly_hours", "bmi", "news_trust", "grades", "burnout_score",
    "exercise_days", "smoking", "screen_time_group", "specialization",
    "contract_type", "health_status", "fact_check", "scholarship",
    "union_member", "exercise_level", "fact_check", "income_bracket",
    "health_status", "grades_band", "education", "age_group", "fact_check",
    "attendance_band", "burnout_level", "volunteer", "access_clinic",
    "fact_check", "first_gen", "contract_type", "age_group", "smoking",
    "platform_primary", "income_bracket", "burnout_level", "volunteer",
    "smoking", "civic_engagement", "bmi", "news_trust", "grades",
    "burnout_score", "civic_engagement", "exercise_days", "screen_hours",
    "study_hours", "training_hours", "trust_gov", "bmi", "screen_hours",
    "study_hours", "job_satisfaction", "age", "age", "attendance_pct",
    "civic_engagement", "exercise_days", "political_interest", "grades",
    "job_satisfaction", "policy_support", "exercise_days", "age",
    "stress_score", "weekly_hours", "trust_gov", "age"), pvalue = c(0.002985,
    5.537e-05, 0.01032, 3.105e-05, 0.01519, 0.0008739, 0.008917,
    0.0006106, 0.001331, 0.006725, 0.5866, 0.08724, 0.8517, 0.8427,
    0.9593, 0.4332, 0.09048, 0.9341, 0.2634, 0.3839, 0.3342, 0.1681,
    0.9858, 0.7565, 0.9762, 0.9404, 0.1356, 0.7938, 0.4106, 0.3186,
    0.01263, 1.743e-06, 0.0123, 0.0004477, 0.003516, 0.04709, 0.0002173,
    0.03264, 0.001097, 0.04101, 0.0005161, 1.715e-05, 0.01926, 0.8593,
    0.7646, 0.6275, 0.07026, 0.3853, 0.05507, 0.05059, 0.7783, 0.2094,
    1, 0.6877, 0.3317, 0.767, 0.7794, 0.9468, 0.2638, 0.6323, 0.00574,
    0, 0, 7.97e-13, 0, 0.0183, 0, 0, 0.00898, 0.00253, 0, 0.000101,
    2.6e-10, 0.046, 0.00266, 0, 0, 0.0406, 0.167, 0.519, 0.229, 0.322,
    0.759, 0.307, 0.548, 0.992, 0.115, 0.322, 0.0641, 0.704)), row.names = c(NA,
    -90L), class = "data.frame")

    if (admisc::possibleNumeric(x)) {
        x <- admisc::asNumeric(x)
        if (any(x < 1) || any(x > nrow(data))) {
            admisc::stopError("The specified test subject IDs out of range.")
        }

    } else {
        admisc::stopError("The input should be a numeric value representing the test subject ID.")
    }

    cat("For each of the following subjects:\n")
    cat("- Formulate a research question, involving the two variables.\n")
    cat("- Establish the variables' measurement levels.\n")
    cat("- Choose the appropriate statistical test to answer the research question.\n")
    cat("- State the null and alternative hypotheses.\n")
    cat("- Perform the test and interpret the results.\n\n")

    for (i in x) {
        cat(paste0(
            "\nSubject ID: ", i,
            "\n  - Dataset: ", data$dataset[i],
            "\n  - Variables: ", data$iv_name[i], ", ", data$dv_name[i], "\n"
        ))
    }
}
