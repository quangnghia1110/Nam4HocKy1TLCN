package studentConsulting.security.authentication;

import com.fasterxml.jackson.annotation.JsonIgnore;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.core.user.OAuth2User;
import studentConsulting.model.entity.UserInformationEntity;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

public class UserPrinciple implements UserDetails, OAuth2User {

    private final String userId;
    private final String email;
    private Map<String, Object> attributes;

    @JsonIgnore
    private final String password;
    private final Collection<? extends GrantedAuthority> authorities;

    public UserPrinciple(String userId, String email, String password, Collection<? extends GrantedAuthority> authorities) {
        this.userId = userId;
        this.email = email;
        this.password = password;
        this.authorities = authorities;
    }

    public static UserPrinciple build(UserInformationEntity userModel) {
        List<GrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority(userModel.getAccount().getRole().getName()));

        return new UserPrinciple(
                userModel.getId().toString(),
                userModel.getAccount().getEmail(),
                userModel.getAccount().getPassword(),
                authorities);
    }

    public static UserPrinciple create(UserInformationEntity user, Map<String, Object> attributes) {
        UserPrinciple userPrincipal = UserPrinciple.build(user);
        userPrincipal.setAttributes(attributes);
        return userPrincipal;
    }

    @Override
    public Collection<? extends GrantedAuthority> getAuthorities() {
        return authorities;
    }

    @Override
    public String getPassword() {
        return password;
    }

    @Override
    public String getUsername() {
        return email;
    }

    @Override
    public boolean isAccountNonExpired() {
        return true;
    }

    @Override
    public boolean isAccountNonLocked() {
        return true;
    }

    @Override
    public boolean isCredentialsNonExpired() {
        return true;
    }

    @Override
    public boolean isEnabled() {
        return true;
    }

    @Override
    public String getName() {
        return email;  // Hoặc có thể trả về một giá trị tên khác tùy vào yêu cầu
    }

    @Override
    public Map<String, Object> getAttributes() {
        return attributes;
    }

    public String getUserId() {
        return userId;
    }

    public String getEmail() {
        return email;
    }

    public void setAttributes(Map<String, Object> attributes) {
        this.attributes = attributes;
    }
}
