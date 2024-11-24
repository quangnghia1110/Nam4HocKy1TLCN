package studentConsulting;

import io.github.cdimascio.dotenv.Dotenv;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.core.env.ConfigurableEnvironment;
import org.springframework.core.env.StandardEnvironment;
import org.springframework.scheduling.annotation.EnableScheduling;

@SpringBootApplication
@EnableScheduling
public class Nam4HocKy1TLCNApplication {
    public static void main(String[] args) {
        // Load environment variables from .env
        Dotenv dotenv = Dotenv.load();

        // Set properties dynamically
        SpringApplication app = new SpringApplication(Nam4HocKy1TLCNApplication.class);
        ConfigurableEnvironment environment = new StandardEnvironment();

        environment.getSystemProperties().put("SERVER_PORT", dotenv.get("SERVER_PORT"));
        environment.getSystemProperties().put("spring.datasource.url", dotenv.get("DB_URL"));
        environment.getSystemProperties().put("spring.datasource.username", dotenv.get("DB_USERNAME"));
        environment.getSystemProperties().put("spring.datasource.password", dotenv.get("DB_PASSWORD"));
        environment.getSystemProperties().put("jwt.secret", dotenv.get("JWT_SECRET"));
        environment.getSystemProperties().put("spring.mail.host", dotenv.get("MAIL_HOST"));
        environment.getSystemProperties().put("spring.mail.port", dotenv.get("MAIL_PORT"));
        environment.getSystemProperties().put("spring.mail.username", dotenv.get("MAIL_USERNAME"));
        environment.getSystemProperties().put("spring.mail.password", dotenv.get("MAIL_PASSWORD"));
        environment.getSystemProperties().put("base.url", dotenv.get("BASE_URL"));

        app.setEnvironment(environment);
        app.run(args);
    }
}
