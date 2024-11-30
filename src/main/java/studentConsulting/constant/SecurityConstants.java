package studentConsulting.constant;

public interface SecurityConstants {

    String[] NOT_JWT = {
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
            "/api/v1/consultation-schedule/list",
            "/api/v1/post/list",
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
            "/api/v1/like-records/comment",
            "/api/v1/like-count/post",
            "/api/v1/like-count/comment",
            "/api/v1/like-records/post",
            "/api/v1/comment/get-comment-by-post",
            "/v3/api-docs/**",
            "/swagger-ui/**",
            "/swagger-ui.html",
    };

    String[] JWT = {
            "/api/v1/consultant/answer/create",
            "/api/v1/advisor-admin/answer/review",
            "/api/v1/answer/update",
            "/api/v1/answer/delete",
            "/api/v1/answer/detail",
            "/api/v1/recall-message-self",
            "/api/v1/recall-message-all",
            "/api/v1/update-message",
            "/api/v1/chat/history",
            "/api/v1/user/conversation/create",
            "/api/v1/consultant/conversation/create",
            "/api/v1/conversation/list",
            "/api/v1/conversation/update",
            "/api/v1/conversation/delete",
            "/api/v1/conversation/detail",
            "/api/v1/conversation/approve-member",
            "/api/v1/consultant/conversation/remove-member",
            "/api/v1/conversation/list-member",
            "/api/v1/conversation/list-users",
            "/api/v1/comment/create",
            "/api/v1/comment/reply",
            "/api/v1/comment/update",
            "/api/v1/comment/delete",
            "/api/v1/advisor-admin/list-common-question",
            "/api/v1/advisor-admin/common-question/convert-to-common",
            "/api/v1/advisor-admin/common-question/update",
            "/api/v1/advisor-admin/common-question/delete",
            "/api/v1/advisor-admin/common-question/detail",
            "/api/v1/user/consultation-schedule/create",
            "/api/v1/consultant/consultation-schedule/confirm",
            "/api/v1/advisor-admin/consultation-schedule/create",
            "/api/v1/consultation-schedule/update",
            "/api/v1/advisor-admin/consultation-schedule/delete",
            "/api/v1/consultation-schedule/detail",
            "/api/v1/user/consultation-schedule/check",
            "/api/v1/user/consultation-schedule/list-join",
            "/api/v1/user/consultation-schedule/join",
            "/api/v1/user/consultation-schedule/cancel",
            "/api/v1/advisor-admin/consultation-schedule/list-member-join",
            "/api/v1/consultant/forward-question/forward",
            "/api/v1/forward-question/list",
            "/api/v1/forward-question/update",
            "/api/v1/forward-question/delete",
            "/api/v1/forward-question/detail",
            "/api/v1/like/post",
            "/api/v1/unlike/post",
            "/api/v1/like/comment",
            "/api/v1/unlike/comment",
            "/api/v1/post",
            "/api/v1/post/update",
            "/api/v1/post/delete",
            "/api/v1/post/detail",
            "/api/v1/user/question/create",
            "/api/v1/question/detail",
            "/api/v1/question/delete",
            "/api/v1/user/question/create-follow-up",
            "/api/v1/question-answer/list",
            "/api/v1/user/question/update",
            "/api/v1/user/question/delete",
            "/api/v1/deletion-log/list",
            "/api/v1/deletion-log/detail",
            "/api/v1/user/rating/create",
            "/api/v1/rating/list",
            "/api/v1/rating/detail",
            "/api/v1/admin/account/list",
            "/api/v1/admin/account/detail",
            "/api/v1/admin/account/update",
            "/api/v1/admin/address/list",
            "/api/v1/admin/address/create",
            "/api/v1/admin/address/update",
            "/api/v1/admin/address/delete",
            "/api/v1/admin/address/detail",
            "/api/v1/admin/department/list",
            "/api/v1/admin/department/create",
            "/api/v1/admin/department/update",
            "/api/v1/admin/department/delete",
            "/api/v1/admin/department/detail",
            "/api/v1/admin/district/list",
            "/api/v1/admin/district/create",
            "/api/v1/admin/district/update",
            "/api/v1/admin/district/delete",
            "/api/v1/admin/district/detail",
            "/api/v1/admin/field/list",
            "/api/v1/admin/field/create",
            "/api/v1/admin/field/update",
            "/api/v1/admin/field/delete",
            "/api/v1/admin/field/detail",
            "/api/v1/admin/post/approve",
            "/api/v1/admin/province/list",
            "/api/v1/admin/province/create",
            "/api/v1/admin/province/update",
            "/api/v1/admin/province/delete",
            "/api/v1/admin/province/detail",
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
            "/api/v1/admin/role/list",
            "/api/v1/admin/role/create",
            "/api/v1/admin/role/update",
            "/api/v1/admin/role/delete",
            "/api/v1/admin/role/detail",
            "/api/v1/admin/user-information/list",
            "/api/v1/admin/user-information/detail",
            "/api/v1/admin/user-information/update",
            "/api/v1/admin/ward/list",
            "/api/v1/admin/ward/create",
            "/api/v1/admin/ward/update",
            "/api/v1/admin/ward/delete",
            "/api/v1/admin/ward/detail",
            "/api/v1/export",
            "/api/v1/import",
            "/api/v1/upload",
            "/api/v1/cloudinary/upload",
            "/api/v1/notification",
            "/api/v1/user/notifications/list",
            "/api/v1/profile/change-password",
            "/api/v1/profile",
            "/api/v1/profile/update",
            "/api/v1/address/provinces",
            "/api/v1/address/districts",
            "/api/v1/address/wards",
            "/api/v1/admin/statistics",
            "/api/v1/admin/statistics/accounts/yearly",
            "/api/v1/admin/statistics/accounts/yearly",
            "/api/v1/admin/statistics/fields/yearly",
            "/api/v1/admin/statistics/role-asks/yearly",
            "/api/v1/admin/statistics/role-consultants/yearly",
            "/api/v1/admin/statistics/roles/yearly",
            "/api/v1/statistics",
            "/api/v1/statistics/questions-deleted/yearly",
            "/api/v1/statistics/questions-forwarded/yearly",
            "/api/v1/statistics/answers-given/yearly",
            "/api/v1/statistics/answer-approval/yearly",
            "/api/v1/statistics/consultation-schedules/yearly",
            "/api/v1/statistics/conversations/yearly",
            "/api/v1/statistics/ratings/yearly",
            "/api/v1/statistics/ratings/yearly",
            "/api/v1/statistics/common-questions/yearly",
            "/api/v1/statistics/consultingByMessage/yearly",
            "/api/v1/user/statistics",
            "/api/v1/user/statistics/questions-status/yearly",
            "/api/v1/user/statistics/ratings/yearly",
            "/api/v1/user/statistics/consultationSchedule/yearly",
            "/api/v1/user/statistics/conversations/yearly",
            "/api/v1/user/statistics/conversationsMember/yearly"
    };

    interface Role {
        String ADMIN = "ROLE_ADMIN";
        String USER = "ROLE_USER";
        String TUVANVIEN = "ROLE_TUVANVIEN";
        String TRUONGBANTUVAN = "ROLE_TRUONGBANTUVAN";
    }

    interface PreAuthorize {
        String ADMIN = "hasRole('ADMIN')";
        String USER = "hasRole('USER')";
        String TUVANVIEN = "hasRole('TUVANVIEN')";
        String TRUONGBANTUVAN = "hasRole('TRUONGBANTUVAN')";
    }

    interface RoleConsultant {
        String GIANGVIEN = "GIANGVIEN";
        String SINHVIEN = "SINHVIEN";
    }
}