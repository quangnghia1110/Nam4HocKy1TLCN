package studentConsulting.security.config;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class OpenApiConfig {

    @Bean
    public OpenAPI customOpenAPI() {
        return new OpenAPI()
        		//Thông tin chung cho API
                .info(new Info().title("Student Consulting API").version("1.0"))
                //Định nghĩa yêu cầu bảo mật API, loại xác thực là bererAuth
                .addSecurityItem(new SecurityRequirement().addList("bearerAuth"))
                //Định nghĩa các thành phần của API, định nghĩa một lược đồ bảo mật có tên "bearerAuth" cho phép xác thực loại "Bearer" sử dụng JWT
                .components(new Components().addSecuritySchemes("bearerAuth",
                        new SecurityScheme().type(SecurityScheme.Type.HTTP).scheme("bearer").bearerFormat("JWT")));
    }
}
