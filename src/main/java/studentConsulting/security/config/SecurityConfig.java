package studentConsulting.security.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import studentConsulting.constant.AppConstants;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.exception.CustomAccessDeniedHandler;
import studentConsulting.model.exception.CustomJWTHandler;
import studentConsulting.security.JWT.JwtEntryPoint;
import studentConsulting.security.JWT.JwtTokenFilter;
import studentConsulting.security.userPrinciple.UserDetailService;

import java.util.Arrays;
import java.util.Collections;

@Configuration
@EnableWebSecurity
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class SecurityConfig extends WebSecurityConfigurerAdapter{

    @Autowired
    private UserDetailService userDetailService;

    @Autowired
    private JwtEntryPoint jwtEntryPoint;

    @Bean
    public JwtTokenFilter jwtTokenFilter() {
        return new JwtTokenFilter();
    }

    @Bean
    PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

    @Autowired
    private CustomAccessDeniedHandler  customAccessDeniedHandler;

    @Autowired
    private CustomJWTHandler customJWTHandler;
    
    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(userDetailService).passwordEncoder(passwordEncoder());
    }

    @Bean
    @Override
    public AuthenticationManager authenticationManager() throws Exception {
        return super.authenticationManager();
    }

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.cors().and().csrf().disable()
            .authorizeRequests()
                .antMatchers("/ws/**").permitAll()  // Cho phép truy cập không cần xác thực cho WebSocket
                .antMatchers(SecurityConstants.IGNORING_API_PATHS).permitAll()
                .anyRequest().authenticated()
            .and()
                .exceptionHandling()
                	.authenticationEntryPoint(jwtEntryPoint)
                    // Xử lý lỗi xác thực với jwtEntryPoint (401 Unauthorized)
                    .authenticationEntryPoint(jwtEntryPoint)
            .and()
                .exceptionHandling()
                	.accessDeniedHandler(customAccessDeniedHandler)
                	.accessDeniedHandler(customJWTHandler)  
            .and()
                // Cấu hình để không dùng session (STATELESS)
                .sessionManagement()
                    .sessionCreationPolicy(SessionCreationPolicy.STATELESS);

        // Đảm bảo jwtTokenFilter được gọi trước UsernamePasswordAuthenticationFilter
        http.addFilterBefore(jwtTokenFilter(), UsernamePasswordAuthenticationFilter.class);
    }

    @Bean
    //Khởi tạo một đối tượng CorsConfiguration mới để cấu hình CORS.
    CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration= new CorsConfiguration();
        configuration.setMaxAge(3600L);
        //Cho phép các yêu cầu từ nguồn gốc http://localhost:3000
        configuration.setAllowedOrigins(Collections.singletonList(AppConstants.FRONTEND_HOST));
        //Cho phép các yêu cầu CORS gửi thông tin xác thực (như cookie, headers xác thực).
        configuration.setAllowCredentials(true);
        //Định nghĩa các headers mà client được phép gửi trong yêu cầu CORS.
        configuration.setAllowedHeaders(Arrays.asList("Access-Control-Allow-Headers","Access-Control-Allow-Origin","Access-Control-Request-Method", "Access-Control-Request-Headers","Origin","Cache-Control", "Content-Type", "Authorization"));
        //Cho phép các phương thức HTTP này trong các yêu cầu CORS: DELETE, GET, POST, PATCH, và PUT.
        configuration.setAllowedMethods(Arrays.asList("DELETE", "GET", "POST", "PATCH", "PUT"));
        final UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        //Đăng ký cấu hình CORS cho tất cả các đường dẫn URL (/**).
        source.registerCorsConfiguration("/**", configuration);
        return source;
    }
    
    
}
