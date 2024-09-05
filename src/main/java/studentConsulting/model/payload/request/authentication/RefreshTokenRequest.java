package studentConsulting.model.payload.request.authentication;

public class RefreshTokenRequest {

    private String refreshToken;
    public RefreshTokenRequest() {
    }

    public String getRefreshToken() {
        return refreshToken;
    }

    public void setRefreshToken(String refreshToken) {
        this.refreshToken = refreshToken;
    }
}
