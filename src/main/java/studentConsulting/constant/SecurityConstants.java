package studentConsulting.constant;

public interface SecurityConstants {

    String[] NOT_JWT = {
            // Authentication & Registration
            "/api/v1/auth/login",
            "/api/v1/auth/register",
            "/api/v1/auth/change-email",
            "/api/v1/auth/resend-register-verification-code",
            "/api/v1/auth/confirm-registration",
            "/api/v1/auth/forgot-password",
            "/api/v1/auth/resend-forgot-password-verification-code",
            "/api/v1/auth/verify-code",
            "/api/v1/auth/reset-password",
            "/api/v1/auth/refresh",

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

    String[] JWT = {
            // Chức năng của admin
            "/api/v1/admin/account/list",
            "/api/v1/admin/account/change-activity",
            "/api/v1/admin/account/detail",
            "/api/v1/admin/user-information/list",
            "/api/v1/admin/user-information/detail",
            "/api/v1/admin/address/list",
            "/api/v1/admin/address/create",
            "/api/v1/admin/address/update",
            "/api/v1/admin/address/delete",
            "/api/v1/admin/address/detail",
            "/api/v1/admin/province/list",
            "/api/v1/admin/province/create",
            "/api/v1/admin/province/update",
            "/api/v1/admin/province/delete",
            "/api/v1/admin/province/detail",
            "/api/v1/admin/district/list",
            "/api/v1/admin/district/create",
            "/api/v1/admin/district/update",
            "/api/v1/admin/district/delete",
            "/api/v1/admin/district/detail",
            "/api/v1/admin/ward/list",
            "/api/v1/admin/ward/create",
            "/api/v1/admin/ward/update",
            "/api/v1/admin/ward/delete",
            "/api/v1/admin/ward/detail",
            "/api/v1/admin/department/list",
            "/api/v1/admin/department/create",
            "/api/v1/admin/department/update",
            "/api/v1/admin/department/delete",
            "/api/v1/admin/department/detail",
            "/api/v1/admin/field/list",
            "/api/v1/admin/field/create",
            "/api/v1/admin/field/update",
            "/api/v1/admin/field/delete",
            "/api/v1/admin/field/detail",
            "/api/v1/admin/role/list",
            "/api/v1/admin/role/create",
            "/api/v1/admin/role/update",
            "/api/v1/admin/role/delete",
            "/api/v1/admin/role/detail",
            "/api/v1/admin/role-ask/list",
            "/api/v1/admin/role-ask/create",
            "/api/v1/admin/role-ask/update",
            "/api/v1/admin/role-ask/delete",
            "/api/v1/admin/role-ask/detail",
            "/api/v1/admin/role-consultant/list",
            "/api/v1/admin/role-consultant/create",
            "/api/v1/admin/role-consultant/update",
            "/api/v1/admin/role-consultant/delete",
            "/api/v1/admin/role-consultant/detail",
            "/api/v1/admin/post/approve",


            // Chức năng của Advisor, Admin
            "/api/v1/advisor-admin/all-deletion-log/list",
            "/api/v1/advisor-admin/answer/delete-answer",
            "/api/v1/advisor-admin/answer/list",
            "/api/v1/advisor-admin/answer/review",
            "/api/v1/advisor-admin/answer/update-answer",
            "/api/v1/advisor-admin/answer/detail",
            "/api/v1/advisor-admin/list-common-question",
            "/api/v1/advisor-admin/common-question/convert-to-common",
            "/api/v1/advisor-admin/common-question/delete",
            "/api/v1/advisor-admin/common-question/update",
            "/api/v1/advisor-admin/common-question/detail",
            "/api/v1/advisor-admin/consultation-schedule-owner/list",
            "/api/v1/advisor-admin/consultation-schedule/list",
            "/api/v1/advisor-admin/consultation-schedule/create",
            "/api/v1/advisor-admin/consultation-schedule/update",
            "/api/v1/advisor-admin/consultation-schedule/delete",
            "/api/v1/advisor-admin/consultation-schedule/list-member-join",
            "/api/v1/advisor-admin/consultation-schedule/detail-owner",
            "/api/v1/advisor-admin/consultation-schedule/detail-consultant",
            "/api/v1/advisor-admin/forward-question/delete",
            "/api/v1/advisor-admin/forward-question/list",
            "/api/v1/advisor-admin/forward-question/update",
            "/api/v1/advisor-admin/forward-question/detail",
            "/api/v1/advisor-admin/question/list-question-by-department",
            "/api/v1/advisor-admin/question/detail",
            "/api/v1/advisor-admin/rating/list",
            "/api/v1/advisor-admin/rating/detail",
            "/api/v1/advisor-admin/consultant/list-consultant",
            "/api/v1/advisor-admin/consultant/update-role-user-to-consultant",

            // Chức năng của consultant
            "/api/v1/consultant/answer/create",
            "/api/v1/consultant/conversation/list",
            "/api/v1/consultant/conversation/create",
            "/api/v1/consultant/conversation/update",
            "/api/v1/consultant/conversation/delete",
            "/api/v1/consultant/conversation/list-detail",
            "/api/v1/consultant/conversation/list-member",
            "/api/v1/consultant/conversation/list-users",
            "/api/v1/consultant/conversation/approve-member",
            "/api/v1/consultant/consultation-schedule/confirm",
            "/api/v1/consultant/consultation-schedule/list",
            "/api/v1/consultant/consultation-schedule/detail",
            "/api/v1/consultant/deletion-log/list",
            "/api/v1/consultant/forward-question/delete",
            "/api/v1/consultant/forward-question/forward",
            "/api/v1/consultant/forward-question/list",
            "/api/v1/consultant/forward-question/update",
            "/api/v1/consultant/question/delete",
            "/api/v1/consultant/question-answer/list",
            "/api/v1/consultant/question/list-delete",

            // Chức năng của User
            "/api/v1/user/consultation-schedule/list-join",
            "/api/v1/user/consultation-schedule/list",
            "/api/v1/user/consultation-schedule/create",
            "/api/v1/user/consultation-schedule/join",
            "/api/v1/user/consultation-schedule/cancel",
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

            // Thống kê của Advisor, Admin
            "/api/v1/advisor-admin/statistics",
            "/api/v1/advisor-admin/statistics/questions-deleted/yearly",
            "/api/v1/advisor-admin/statistics/answers-given/yearly",
            "/api/v1/advisor-admin/statistics/answer-approval/yearly",
            "/api/v1/advisor-admin/statistics/consultation-schedules/yearly",
            "/api/v1/advisor-admin/statistics/conversations/yearly",
            "/api/v1/advisor-admin/statistics/ratings/yearly",
            "/api/v1/advisor-admin/statistics/common-questions/yearly",

            // Thống kê của Consultant
            "/api/v1/consultant/statistics",
            "/api/v1/consultant/statistics/answer-approvals",
            "/api/v1/consultant/statistics/answers-given",
            "/api/v1/consultant/statistics/approved-posts",
            "/api/v1/consultant/statistics/conversations",
            "/api/v1/consultant/statistics/consultation-schedules",
            "/api/v1/consultant/statistics/deleted-questions",

            // Thống kê của User
            "/api/v1/user/statistics",
            "/api/v1/user/statistics/conversations/yearly",
            "/api/v1/user/statistics/conversationsMember/yearly",
            "/api/v1/user/statistics/consultationSchedule/yearly",
            "/api/v1/user/statistics/questions-status/yearly",
            "/api/v1/user/statistics/ratings/yearly",

            // Export, Import của Admin
            "/api/v1/admin/export-account-csv",
            "/api/v1/admin/export-account-pdf",
            "/api/v1/admin/import-account-csv",
            "/api/v1/admin/export-user-information-csv",
            "/api/v1/admin/export-user-information-pdf",
            "/api/v1/admin/import-user-information-csv",
            "/api/v1/admin/export-address-csv",
            "/api/v1/admin/export-address-pdf",
            "/api/v1/admin/import-address-csv",
            "/api/v1/admin/export-provinces-csv",
            "/api/v1/admin/export-provinces-pdf",
            "/api/v1/admin/import-provinces-csv",
            "/api/v1/admin/export-district-csv",
            "/api/v1/admin/export-district-pdf",
            "/api/v1/admin/import-district-csv",
            "/api/v1/admin/export-ward-csv",
            "/api/v1/admin/export-ward-pdf",
            "/api/v1/admin/import-ward-csv",
            "/api/v1/admin/export-departments-csv",
            "/api/v1/admin/export-departments-pdf",
            "/api/v1/admin/import-departments-csv",
            "/api/v1/admin/export-fields-csv",
            "/api/v1/admin/export-fields-pdf",
            "/api/v1/admin/import-fields-csv",
            "/api/v1/admin/export-role-ask-csv",
            "/api/v1/admin/export-role-ask-pdf",
            "/api/v1/admin/import-role-ask-csv",
            "/api/v1/admin/export-role-consultant-csv",
            "/api/v1/admin/export-role-consultant-pdf",
            "/api/v1/admin/import-role-consultant-csv",
            "/api/v1/admin/export-role-csv",
            "/api/v1/admin/export-role-pdf",
            "/api/v1/admin/import-role-csv",
            "/api/v1/admin/export-post-csv",
            "/api/v1/admin/export-post-pdf",
            "/api/v1/admin/import-post-csv",

            // Export, Import của Advisor
            "/api/v1/advisor-admin/export-answer-csv",
            "/api/v1/advisor-admin/export-answer-pdf",
            "/api/v1/advisor-admin/import-answer-csv",
            "/api/v1/advisor-admin/export-common-question-csv",
            "/api/v1/advisor-admin/export-common-question-pdf",
            "/api/v1/advisor-admin/import-common-question-csv",
            "/api/v1/advisor-admin/export-consultation-schedule-csv",
            "/api/v1/advisor-admin/export-consultation-schedule-pdf",
            "/api/v1/advisor-admin/import-consultation-schedule-csv",
            "/api/v1/advisor-admin/export-consultation-schedule-owner-csv",
            "/api/v1/advisor-admin/export-consultation-schedule-owner-pdf",
            "/api/v1/advisor-admin/import-consultation-schedule-owner-csv",
            "/api/v1/advisor-admin/export-conversation-csv",
            "/api/v1/advisor-admin/export-conversation-pdf",
            "/api/v1/advisor-admin/import-conversation-csv",
            "/api/v1/advisor-admin/export-forward-question-csv",
            "/api/v1/advisor-admin/export-forward-question-pdf",
            "/api/v1/advisor-admin/import-forward-question-csv",
            "/api/v1/advisor-admin/export-question-csv",
            "/api/v1/advisor-admin/export-question-pdf",
            "/api/v1/advisor-admin/import-question-csv",
            "/api/v1/advisor-admin/export-rating-csv",
            "/api/v1/advisor-admin/export-rating-pdf",

            // Chức năng Post của Consultant, Advisor, Admin
            "/api/v1/post",
            "/api/v1/post/delete",
            "/api/v1/post/list",
            "/api/v1/post/update",

            // Chức năng thông tin cá nhân
            "/api/v1/profile",
            "/api/v1/profile/change-password",
            "/api/v1/profile/update",

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

    interface RoleConsultant {
        String GIANGVIEN = "GIANGVIEN";
        String SINHVIEN = "SINHVIEN";
    }
}