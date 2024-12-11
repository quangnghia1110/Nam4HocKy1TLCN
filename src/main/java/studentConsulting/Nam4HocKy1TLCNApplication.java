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
        Dotenv dotenv = null;

        boolean isRailwayEnv = System.getenv("RAILWAY_ENV") != null;

        if (!isRailwayEnv) {
            dotenv = Dotenv.configure()
                    .filename(".env")
                    .ignoreIfMissing()
                    .load();
        } else {
            throw new RuntimeException("CRunning in Railway environment. Using environment variables");
        }

        SpringApplication app = new SpringApplication(Nam4HocKy1TLCNApplication.class);
        ConfigurableEnvironment environment = new StandardEnvironment();

        environment.getSystemProperties().put("SERVER_PORT",
                isRailwayEnv ? System.getenv("SERVER_PORT") : dotenv != null ? dotenv.get("SERVER_PORT") : "8080");

        environment.getSystemProperties().put("spring.datasource.url",
                isRailwayEnv ? System.getenv("DB_URL") : dotenv != null ? dotenv.get("DB_URL") : "");

        environment.getSystemProperties().put("spring.datasource.username",
                isRailwayEnv ? System.getenv("DB_USERNAME") : dotenv != null ? dotenv.get("DB_USERNAME") : "");

        environment.getSystemProperties().put("spring.datasource.password",
                isRailwayEnv ? System.getenv("DB_PASSWORD") : dotenv != null ? dotenv.get("DB_PASSWORD") : "");

        environment.getSystemProperties().put("jwt.secret",
                isRailwayEnv ? System.getenv("JWT_SECRET") : dotenv != null ? dotenv.get("JWT_SECRET") : "");

        environment.getSystemProperties().put("spring.mail.host",
                isRailwayEnv ? System.getenv("MAIL_HOST") : dotenv != null ? dotenv.get("MAIL_HOST") : "");

        environment.getSystemProperties().put("spring.mail.port",
                isRailwayEnv ? System.getenv("MAIL_PORT") : dotenv != null ? dotenv.get("MAIL_PORT") : "");

        environment.getSystemProperties().put("spring.mail.username",
                isRailwayEnv ? System.getenv("MAIL_USERNAME") : dotenv != null ? dotenv.get("MAIL_USERNAME") : "");

        environment.getSystemProperties().put("spring.mail.password",
                isRailwayEnv ? System.getenv("MAIL_PASSWORD") : dotenv != null ? dotenv.get("MAIL_PASSWORD") : "");

        environment.getSystemProperties().put("base.url",
                isRailwayEnv ? System.getenv("BASE_URL") : dotenv != null ? dotenv.get("BASE_URL") : "");

//        environment.getSystemProperties().put("spring.security.oauth2.client.registration.google.client-id",
//                isRailwayEnv ? System.getenv("GOOGLE_CLIENT_ID") : dotenv != null ? dotenv.get("GOOGLE_CLIENT_ID") : "");
//
//        environment.getSystemProperties().put("spring.security.oauth2.client.registration.google.client-secret",
//                isRailwayEnv ? System.getenv("GOOGLE_CLIENT_SECRET") : dotenv != null ? dotenv.get("GOOGLE_CLIENT_SECRET") : "");
//
//        environment.getSystemProperties().put("spring.security.oauth2.client.registration.google.redirect-uri",
//                isRailwayEnv ? System.getenv("REDIRECT_URI") : dotenv != null ? dotenv.get("REDIRECT_URI") : "");
//
//        environment.getSystemProperties().put("app.oauth2.authorizedRedirectUris",
//                isRailwayEnv ? System.getenv("AUTHORIZED_REDIRECT_URI") : dotenv != null ? dotenv.get("AUTHORIZED_REDIRECT_URI") : "");

        app.setEnvironment(environment);
        app.run(args);
    }
}
