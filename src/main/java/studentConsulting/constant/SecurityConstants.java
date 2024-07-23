package studentConsulting.constant;

public interface SecurityConstants {
    String[] ADMIN_API_PATHS = {
    		"/api/v1/admin/**"
    };

    String[] USER_API_PATHS = {
    		"/api/v1/user/**"
    };

    String[] IGNORING_API_PATHS = {
    		"/api/v1/addresses/**", 
    		"/api/v1/auth/**", 
    		"/v3/api-docs/**", 
    		"/swagger-ui/**", 
    		"/swagger-ui.html"
    };

    interface Role {
        String ADMIN = "ADMIN";
        String USER = "USER";
    }
}
