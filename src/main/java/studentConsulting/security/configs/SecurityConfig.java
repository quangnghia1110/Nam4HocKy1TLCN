//CÁI NÀY THỰC HIỆN THỨ 2

package studentConsulting.security.configs;

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
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.CorsConfigurationSource;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;

import studentConsulting.security.JWT.JwtEntryPoint;
import studentConsulting.security.JWT.JwtTokenFilter;
import studentConsulting.security.UserPrinciple.UserDetailService;

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
    public JwtTokenFilter jwtTokenFilter()
    {
        return new JwtTokenFilter();
    }


    @Bean
    PasswordEncoder passwordEncoder()
    {
        return new BCryptPasswordEncoder();
    }


    @Override
    protected void configure(AuthenticationManagerBuilder auth) throws Exception {
        auth.userDetailsService(userDetailService).passwordEncoder(passwordEncoder());
    }

    @Bean
    @Override
    public AuthenticationManager authenticationManager() throws Exception{
        return super.authenticationManager();
    }


    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http.cors().configurationSource(corsConfigurationSource()).and().csrf().disable()
                .authorizeHttpRequests()
                .antMatchers("/api/auth/**", "/v3/api-docs/**", "/swagger-ui/**").permitAll()
                .antMatchers("/api/user/**").hasAuthority("USER")
                .antMatchers("/api/admin/**").hasAuthority("ADMIN")
                .anyRequest().authenticated()
                .and().exceptionHandling()
                //Đặt giá trị jwtEntryPoint làm AuthenticationEntryPoint cho toàn ứng dụng
                //Mục đích là khi 1 request không xác thực được gửi đến thì nó sẽ phản hồi lại
                //Cụ thể là Unauthorized
                .authenticationEntryPoint(jwtEntryPoint)
                .and().sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);

        //jwtTokenFilter sẽ được gọi trước khi kiểm tra và xác thực JWT trước khi bất kỳ hành động nào được thực hiện
        http.addFilterBefore(jwtTokenFilter(), UsernamePasswordAuthenticationFilter.class);
    }

    @Bean
    //Khởi tạo một đối tượng CorsConfiguration mới để cấu hình CORS.
    CorsConfigurationSource corsConfigurationSource() {
        CorsConfiguration configuration= new CorsConfiguration();
        configuration.setMaxAge(3600L);
        //Cho phép các yêu cầu từ nguồn gốc http://localhost:3000
        configuration.setAllowedOrigins(Collections.singletonList("http://localhost:3000"));
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
