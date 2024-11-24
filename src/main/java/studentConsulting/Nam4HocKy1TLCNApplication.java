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
        // Tải file .env chỉ khi chạy local
//        Dotenv dotenv = null;
//        if (System.getenv("RAILWAY_ENV") == null) { // Railway có thể tự động đặt biến môi trường như "RAILWAY_ENV"
//            dotenv = Dotenv.configure()
//                    .filename(".env") // Tải file .env
//                    .ignoreIfMissing()
//                    .load();
//        }

        SpringApplication app = new SpringApplication(Nam4HocKy1TLCNApplication.class);
//        ConfigurableEnvironment environment = new StandardEnvironment();
//
//        // Đọc biến từ môi trường hệ thống (System.getenv()) hoặc từ dotenv
//        environment.getSystemProperties().put("SERVER_PORT",
//                System.getenv("SERVER_PORT") != null ? System.getenv("SERVER_PORT") : dotenv != null ? dotenv.get("SERVER_PORT") : "8080");
//
//        environment.getSystemProperties().put("spring.datasource.url",
//                System.getenv("DB_URL") != null ? System.getenv("DB_URL") : dotenv != null ? dotenv.get("DB_URL") : "");
//
//        environment.getSystemProperties().put("spring.datasource.username",
//                System.getenv("DB_USERNAME") != null ? System.getenv("DB_USERNAME") : dotenv != null ? dotenv.get("DB_USERNAME") : "");
//
//        environment.getSystemProperties().put("spring.datasource.password",
//                System.getenv("DB_PASSWORD") != null ? System.getenv("DB_PASSWORD") : dotenv != null ? dotenv.get("DB_PASSWORD") : "");
//
//        environment.getSystemProperties().put("jwt.secret",
//                System.getenv("JWT_SECRET") != null ? System.getenv("JWT_SECRET") : dotenv != null ? dotenv.get("JWT_SECRET") : "");
//
//        environment.getSystemProperties().put("spring.mail.host",
//                System.getenv("MAIL_HOST") != null ? System.getenv("MAIL_HOST") : dotenv != null ? dotenv.get("MAIL_HOST") : "");
//
//        environment.getSystemProperties().put("spring.mail.port",
//                System.getenv("MAIL_PORT") != null ? System.getenv("MAIL_PORT") : dotenv != null ? dotenv.get("MAIL_PORT") : "");
//
//        environment.getSystemProperties().put("spring.mail.username",
//                System.getenv("MAIL_USERNAME") != null ? System.getenv("MAIL_USERNAME") : dotenv != null ? dotenv.get("MAIL_USERNAME") : "");
//
//        environment.getSystemProperties().put("spring.mail.password",
//                System.getenv("MAIL_PASSWORD") != null ? System.getenv("MAIL_PASSWORD") : dotenv != null ? dotenv.get("MAIL_PASSWORD") : "");
//
//        environment.getSystemProperties().put("base.url",
//                System.getenv("BASE_URL") != null ? System.getenv("BASE_URL") : dotenv != null ? dotenv.get("BASE_URL") : "");
//
//        app.setEnvironment(environment);
        app.run(args);
    }
}
