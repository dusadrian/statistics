`subjects` <- function() {
    UseMethod ("subjects")
}

`subjects.default` <- function() {
    data <- structure(list(dataset = c("civic_attitudes.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "students.rds",
"workplace.rds", "workplace.rds", "workplace.rds", "civic_attitudes.rds",
"community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
"civic_attitudes.rds", "community_health.rds", "media_use.rds",
"students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "community_health.rds",
"media_use.rds", "community_health.rds", "media_use.rds", "students.rds",
"workplace.rds", "community_health.rds", "media_use.rds", "students.rds",
"workplace.rds", "community_health.rds", "media_use.rds", "students.rds",
"community_health.rds", "students.rds", "civic_attitudes.rds",
"community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
"civic_attitudes.rds", "community_health.rds", "media_use.rds",
"students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
"community_health.rds", "civic_attitudes.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
"community_health.rds", "students.rds", "workplace.rds", "students.rds",
"students.rds", "students.rds", "civic_attitudes.rds", "community_health.rds",
"media_use.rds", "students.rds", "workplace.rds", "civic_attitudes.rds",
"community_health.rds", "media_use.rds", "students.rds", "workplace.rds",
"civic_attitudes.rds", "community_health.rds", "media_use.rds",
"students.rds", "workplace.rds", "civic_attitudes.rds", "community_health.rds",
"media_use.rds"), var_a = c("volunteer", "access_clinic", "fact_check",
"scholarship", "union_member", "first_gen", "contract_type",
"contract_type", "union_member", "volunteer", "access_clinic",
"fact_check", "first_gen", "union_member", "volunteer", "smoking",
"gender", "scholarship", "union_member", "volunteer", "smoking",
"gender", "gender", "contract_type", "access_clinic", "gender",
"scholarship", "union_member", "smoking", "gender", "bmi_category",
"news_trust_group", "specialization", "sector", "health_status",
"platform_primary", "study_hours_level", "sector", "exercise_level",
"platform_primary", "first_gen", "access_clinic", "gender", "education",
"bmi_category", "platform_primary", "first_gen", "union_member",
"vote_intent", "neighborhood", "political_interest", "gender",
"contract_type", "residency", "access_clinic", "gender", "specialization",
"weekly_hours_group", "vote_intent", "neighborhood", "residency",
"health_status", "platform_primary", "specialization", "sector",
"vote_intent", "health_status", "attendance_band", "sector",
"grades_band", "income_bracket", "specialization", "trust_group",
"exercise_level", "news_trust_group", "specialization", "sector",
"trust_group", "age_group", "political_interest", "attendance_band",
"weekly_hours_group", "education", "bmi_category", "political_interest",
"study_hours_level", "burnout_level", "residency", "neighborhood",
"news_trust_group"), var_b = c("civic_engagement", "exercise_days",
"news_trust", "study_hours", "training_hours", "stress_score",
"training_hours", "job_satisfaction", "burnout_score", "policy_support",
"age", "screen_hours", "attendance_pct", "job_satisfaction",
"trust_gov", "age", "age", "attendance_pct", "weekly_hours",
"age", "bmi", "screen_hours", "attendance_pct", "burnout_score",
"bmi", "news_trust", "stress_score", "remote_days", "exercise_days",
"misinformation_score", "health_status", "fact_check", "grades_band",
"contract_type", "smoking", "fact_check", "scholarship", "union_member",
"health_status", "screen_time_group", "income_bracket", "exercise_level",
"specialization", "trust_group", "age_group", "news_trust_group",
"attendance_band", "weekly_hours_group", "education", "access_clinic",
"screen_time_group", "attendance_band", "burnout_level", "volunteer",
"smoking", "news_trust_group", "study_hours_level", "burnout_level",
"age_group", "smoking", "civic_engagement", "bmi", "screen_hours",
"grades", "job_satisfaction", "civic_engagement", "exercise_days",
"study_hours", "training_hours", "study_hours", "stress_score",
"attendance_pct", "age", "age", "screen_hours", "stress_score",
"weekly_hours", "policy_support", "bmi", "screen_hours", "grades",
"remote_days", "age", "exercise_days", "age", "stress_score",
"training_hours", "trust_gov", "exercise_days", "misinformation_score"
)), class = "data.frame", row.names = c(NA, -90L))

    cat("\n")
    cat("For each of the following subjects:\n")
    cat("- Formulate a research question, involving the two variables.\n")
    cat("- Establish the variables' measurement levels.\n")
    cat("- Choose the appropriate statistical test to answer the research question.\n")
    cat("- State the null and alternative hypotheses.\n")
    cat("- Perform the test and interpret the results.\n")

    for (i in sample(1:90, 3)) {
        cat(paste0(
            "\nSubject ID: ", i,
            "\n  - Dataset: ", data$dataset[i],
            "\n  - Variables: ", data$var_a[i], ", ", data$var_b[i], "\n\n"
        ))
    }
}
