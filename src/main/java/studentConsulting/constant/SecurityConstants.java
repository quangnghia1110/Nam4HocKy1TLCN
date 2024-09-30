package studentConsulting.constant;

public interface SecurityConstants {

    String[] NOT_JWT = {

            "/api/v1/auth/verify-code",
            "/api/v1/auth/reset-password",
            "/api/v1/auth/resend-register-verification-code",
            "/api/v1/auth/resend-forgot-password-verification-code",
            "/api/v1/auth/register",
            "/api/v1/auth/login",
            "/api/v1/auth/forgot-password",
            "/api/v1/auth/confirm-registration",
            "/api/v1/auth/change-email",
            "/api/v1/auth/refresh",

            "api/v1/chat/history",
            "api/v1/notification",

            "/api/v1/address/wards",
            "/api/v1/address/provinces",
            "/api/v1/address/districts",

            "/api/v1/list-question",
            "/api/v1/list-common-question",
            "/api/v1/list-consultant",
            "/api/v1/list-consultant-by-department",
            "/api/v1/list-department",
            "/api/v1/list-field-by-department",
            "/api/v1/list-filter-status-options",
            "/api/v1/list-consultant-teacher-by-department",
            "/api/v1/list-consultant-student-by-department",

            "/v3/api-docs/**",
            "/swagger-ui/**",
            "/swagger-ui.html",
    };

    String[] JWT = {

            "/api/v1/admin",
            "/api/v1/admin/post/approve",

            "/api/v1/advisor/all-deletion-log/list",
            "/api/v1/advisor/answer/delete-answer",
            "/api/v1/advisor/answer/list-all-answers",
            "/api/v1/advisor/answer/list-answer-approved",
            "/api/v1/advisor/answer/review",
            "/api/v1/advisor/answer/update-answer",
            "/api/v1/advisor/common-question/convert-to-common",
            "/api/v1/advisor/common-question/delete",
            "/api/v1/advisor/common-question/update",
            "/api/v1/advisor/consultant/list-consultant",
            "/api/v1/advisor/consultant/update-role-consultant-to-user",
            "/api/v1/advisor/consultation-schedule-owner/list",
            "/api/v1/advisor/consultation-schedule/create",
            "/api/v1/advisor/consultation-schedule/delete",
            "/api/v1/advisor/consultation-schedule/list",
            "/api/v1/advisor/consultation-schedule/list-member-join",
            "/api/v1/advisor/consultation-schedule/update",
            "/api/v1/advisor/forward-question/delete",
            "/api/v1/advisor/forward-question/list",
            "/api/v1/advisor/forward-question/update",
            "/api/v1/advisor/list-common-question",
            "/api/v1/advisor/question/list-question-by-department",
            "/api/v1/advisor/rating/list",

            "/api/v1/comment/create",
            "/api/v1/comment/delete",
            "/api/v1/comment/get-comment-by-post",
            "/api/v1/comment/reply",

            "/api/v1/consultant/answer/create",
            "/api/v1/consultant/conversation/create",
            "/api/v1/consultant/conversation/delete",
            "/api/v1/consultant/conversation/list",
            "/api/v1/consultant/conversation/list-detail",
            "/api/v1/consultant/conversation/list-member",
            "/api/v1/consultant/conversation/update",
            "/api/v1/consultant/consultation-schedule/confirm",
            "/api/v1/consultant/consultation-schedule/list",
            "/api/v1/consultant/consultation-schedule/update",
            "/api/v1/consultant/deletion-log/list",
            "/api/v1/consultant/forward-question/delete",
            "/api/v1/consultant/forward-question/forward",
            "/api/v1/consultant/forward-question/list",
            "/api/v1/consultant/forward-question/update",
            "/api/v1/consultant/group/approve-member",
            "/api/v1/consultant/question/delete",
            "/api/v1/consultant/question/list-answer",
            "/api/v1/consultant/question/list-delete",

            "/api/v1/consultant/statistics",
            "/api/v1/consultant/statistics/answer-approvals",
            "/api/v1/consultant/statistics/answers-given",
            "/api/v1/consultant/statistics/approved-posts",
            "/api/v1/consultant/statistics/conversations",
            "/api/v1/consultant/statistics/consultation-schedules",
            "/api/v1/consultant/statistics/deleted-questions",

            "/api/v1/post",
            "/api/v1/post/delete",
            "/api/v1/post/pending",
            "/api/v1/post/update",

            "/api/v1/profile",
            "/api/v1/profile/change-password",
            "/api/v1/profile/update",

            "/api/v1/user/consultation-schedule/cancel",
            "/api/v1/user/consultation-schedule/create",
            "/api/v1/user/consultation-schedule/join",
            "/api/v1/user/consultation-schedule/list",
            "/api/v1/user/consultation-schedule/list-join",
            "/api/v1/user/conversation/create",
            "/api/v1/user/conversation/delete",
            "/api/v1/user/conversation/list",
            "/api/v1/user/conversation/list-detail",
            "/api/v1/user/conversation/update",
            "/api/v1/user/question/create",
            "/api/v1/user/question/create-follow-up",
            "/api/v1/user/question/delete",
            "/api/v1/user/question/list",
            "/api/v1/user/question/role-ask",
            "/api/v1/user/question/update",
            "/api/v1/user/rating",
            "/api/v1/user/rating/create",
            "/api/v1/user/rating/list",

            "/api/v1/user/statistics",
            "/api/v1/user/statistics/conversations/yearly",
            "/api/v1/user/statistics/conversationsMember/yearly",
            "/api/v1/user/statistics/consultationSchedule/yearly",
            "/api/v1/user/statistics/questions-status/yearly",
            "/api/v1/user/statistics/ratings/yearly"
    };


    interface Role {
        String ADMIN = "ROLE_ADMIN";
        String USER = "ROLE_USER";
        String TUVANVIEN = "ROLE_TUVANVIEN";
        String TRUONGBANTUVAN = "ROLE_TRUONGBANTUVAN";
        String GUEST = "ROLE_GUEST";
    }

    interface PreAuthorize {
        String ADMIN = "hasRole('ADMIN')";
        String USER = "hasRole('USER')";
        String TUVANVIEN = "hasRole('TUVANVIEN')";
        String TRUONGBANTUVAN = "hasRole('TRUONGBANTUVAN')";
        String GUEST = "hasRole('GUEST')";
    }
}





