package studentConsulting.constant;

public interface SecurityConstants {

    String[] ADMIN_API_PATHS = {
    	"/api/v1/admin",
        "/api/v1/admin/post/approve",
        
    };

    String[] USER_API_PATHS = {
        "/api/v1/user/consultation-schedule/create",
        "/api/v1/user/consultation-schedule/list",
        "/api/v1/user/conversation/create",
        "/api/v1/user/conversation/list",
        "/api/v1/user/conversation/list-detail",
        "/api/v1/user/question/create",
        "/api/v1/user/question/update",
        "/api/v1/user/question/delete",
        "/api/v1/user/question/create-follow-up",
        "/api/v1/user/question/role-ask",
        "/api/v1/user/question/list",
        "/api/v1/user/rating/create",
        "/api/v1/user/rating/list",
    };

    String[] TUVANVIEN_API_PATHS = {
        "/api/v1/consultant/answer/create",
    	"/api/v1/consultant/consultation-schedule/list",
        "/api/v1/consultant/consultation-schedule/confirm",
    	"/api/v1/consultant/question/list-answer",
        "/api/v1/consultant/question/list-delete",
        "/api/v1/consultant/question/delete",
    	"/api/v1/consultant/question/forward",
        "/api/v1/consultant/question/list-forward",
        "/api/v1/post",
        "/api/v1/post/pending",
        "/api/v1/post/delete",
        "/api/v1/post/update",        
    };

    String[] TRUONGBANTUVAN_API_PATHS = {
    	"/api/v1/advisor/answer/review",
        "/api/v1/advisor/common-question/convert-to-common",
        "/api/v1/advisor/question/list-question-by-department",
        "/api/v1/post",
        "/api/v1/post/pending",
        "/api/v1/post/delete",
        "/api/v1/post/update",        
    };

    String[] IGNORING_API_PATHS = {
    	"/api/v1/profile/update",
        "/api/v1/profile/change-password",
        "/api/v1/profile",
        
        "/api/v1/auth/verify-code",
        "/api/v1/auth/reset-password",
        "/api/v1/auth/resend-register-verification-code",
        "/api/v1/auth/resend-forgot-password-verification-code",
        "/api/v1/auth/register",
        "/api/v1/auth/refresh",
        "/api/v1/auth/login",
        "/api/v1/auth/forgot-password",
        "/api/v1/auth/confirm-registration",
        "/api/v1/auth/change-email",
        
        "api/v1/chat/history",
        
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
        
        "/api/v1/comment/create",
        "/api/v1/comment/reply",
        "/api/v1/comment/get-comment-by-post",
        "/api/v1/comment/delete",
        
        "/v3/api-docs/**",
        "/swagger-ui/**",
        "/swagger-ui.html",
    };

    interface Role {
        String ADMIN = "ROLE_ADMIN";
        String USER = "ROLE_USER";
        String TUVANVIEN = "ROLE_TUVANVIEN";
        String TRUONGBANTUVAN = "ROLE_TRUONGBANTUVAN";
        String GUEST = "ROLE_GUEST";
    }
}




