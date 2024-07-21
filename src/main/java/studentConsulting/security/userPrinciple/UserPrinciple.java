//CÁI NÀY THỰC HIỆN THỨ 5
//UserDetailService SẼ GỌI ĐẾN bulid
package studentConsulting.security.userPrinciple;


import com.fasterxml.jackson.annotation.JsonIgnore;

import studentConsulting.model.entity.authentication.UserInformationEntity;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

// interface UserDetails được sử dụng để đại diện cho thông tin người
public class UserPrinciple implements UserDetails {

    private String userId;

    private String username;
    @JsonIgnore
    private String password;

    public Collection<? extends GrantedAuthority> authorities;

    public UserPrinciple(String userId, String username, String password, Collection<? extends GrantedAuthority> authorities) {
        this.userId = userId;
        this.username = username;
        this.password = password;
        this.authorities = authorities;
    }

    public static UserPrinciple build(UserInformationEntity userModel)
    {
        List<GrantedAuthority> authorities = new ArrayList<>();
        authorities.add(new SimpleGrantedAuthority(userModel.getAccountModel().getRoleModel().getName()));
        return new UserPrinciple(
                userModel.getId().toString(),
                userModel.getAccountModel().getUsername(),
                userModel.getAccountModel().getPassword(),
                authorities);
    }

    public UserPrinciple() {
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public void setAuthorities(Collection<? extends GrantedAuthority> authorities) {
        this.authorities = authorities;
    }

    public String getUserId() {
        return userId;
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
        return username;
    }

    @Override
    public boolean isAccountNonExpired() {
        return true ;
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
}
