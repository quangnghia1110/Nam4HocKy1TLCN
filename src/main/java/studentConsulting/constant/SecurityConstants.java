package studentConsulting.constant;

public interface SecurityConstants {

    String[] NOT_JWT = {
            // Authentication & Registration
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

            // Chat & Notification
            "/api/v1/chat/history",
            "/api/v1/notification",
            "/api/v1/update-message",
            "/api/v1/recall-message-all",
            "/api/v1/recall-message-self",
            "/api/v1/group-message",
            "/api/v1/private-message",

            // Address
            "/api/v1/address/wards",
            "/api/v1/address/provinces",
            "/api/v1/address/districts",

            // Public Question & Consultant Data
            "/api/v1/list-question",
            "/api/v1/list-common-question",
            "/api/v1/list-consultant",
            "/api/v1/list-consultant-by-department",
            "/api/v1/list-department",
            "/api/v1/list-field-by-department",
            "/api/v1/list-filter-status-options",
            "/api/v1/list-consultant-teacher-by-department",
            "/api/v1/list-consultant-student-by-department",
            "/api/v1/list-consultant-rating-by-department",

            // API Documentation
            "/v3/api-docs/**",
            "/swagger-ui/**",
            "/swagger-ui.html",
    };

    // Endpoints requiring JWT
    String[] JWT = {
            // Admin Management
            "/api/v1/admin/post/approve",

            // Advisor Admin Operations
            "/api/v1/advisor-admin/all-deletion-log/list",
            "/api/v1/advisor-admin/answer/delete-answer",
            "/api/v1/advisor-admin/answer/list-all-answers",
            "/api/v1/advisor-admin/answer/list-answer-approved",
            "/api/v1/advisor-admin/answer/review",
            "/api/v1/advisor-admin/answer/update-answer",
            "/api/v1/advisor-admin/answer/detail",
            "/api/v1/advisor-admin/common-question/convert-to-common",
            "/api/v1/advisor-admin/common-question/delete",
            "/api/v1/advisor-admin/common-question/update",
            "/api/v1/advisor-admin/common-question/detail",
            "/api/v1/advisor-admin/consultant/list-consultant",
            "/api/v1/advisor-admin/consultant/update-role-consultant-to-user",
            "/api/v1/advisor-admin/consultation-schedule-owner/list",
            "/api/v1/advisor-admin/consultation-schedule/create",
            "/api/v1/advisor-admin/consultation-schedule/delete",
            "/api/v1/advisor-admin/consultation-schedule/list",
            "/api/v1/advisor-admin/consultation-schedule/list-member-join",
            "/api/v1/advisor-admin/consultation-schedule/update",
            "/api/v1/advisor-admin/consultation-schedule/detail-owner",
            "/api/v1/advisor-admin/consultation-schedule/detail-consultant",
            "/api/v1/advisor-admin/forward-question/delete",
            "/api/v1/advisor-admin/forward-question/list",
            "/api/v1/advisor-admin/forward-question/update",
            "/api/v1/advisor-admin/forward-question/detail",
            "/api/v1/advisor-admin/list-common-question",
            "/api/v1/advisor-admin/question/list-question-by-department",
            "/api/v1/advisor-admin/question/detail",
            "/api/v1/advisor-admin/rating/list",
            "/api/v1/advisor-admin/rating/detail",
            "/api/v1/advisor-admin/rating/summary/excel",
            "/api/v1/advisor-admin/rating/summary/pdf",

            // Consultant Operations
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

            // Consultant Statistics
            "/api/v1/consultant/statistics",
            "/api/v1/consultant/statistics/answer-approvals",
            "/api/v1/consultant/statistics/answers-given",
            "/api/v1/consultant/statistics/approved-posts",
            "/api/v1/consultant/statistics/conversations",
            "/api/v1/consultant/statistics/consultation-schedules",
            "/api/v1/consultant/statistics/deleted-questions",

            // Post Operations
            "/api/v1/post",
            "/api/v1/post/delete",
            "/api/v1/post/pending",
            "/api/v1/post/update",

            // User Profile
            "/api/v1/profile",
            "/api/v1/profile/change-password",
            "/api/v1/profile/update",

            // User Operations
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

            // User Statistics
            "/api/v1/user/statistics",
            "/api/v1/user/statistics/conversations/yearly",
            "/api/v1/user/statistics/conversationsMember/yearly",
            "/api/v1/user/statistics/consultationSchedule/yearly",
            "/api/v1/user/statistics/questions-status/yearly",
            "/api/v1/user/statistics/ratings/yearly",

            // Advisor Statistics
            "/api/v1/advisor/statistics",
            "/api/v1/advisor/statistics/questions-deleted/yearly",
            "/api/v1/advisor/statistics/answers-given/yearly",
            "/api/v1/advisor/statistics/answer-approval/yearly",
            "/api/v1/advisor/statistics/consultation-schedules/yearly",
            "/api/v1/advisor/statistics/conversations/yearly",
            "/api/v1/advisor/statistics/ratings/yearly",
            "/api/v1/advisor/statistics/common-questions/yearly",

            // Export/Import Operations
            "/api/v1/export-departments-csv",
            "/api/v1/export-fields-csv",
            "/api/v1/export-departments-pdf",
            "/api/v1/export-fields-pdf",
            "/api/v1/import-departments-csv",
            "/api/v1/import-fields-csv",
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
