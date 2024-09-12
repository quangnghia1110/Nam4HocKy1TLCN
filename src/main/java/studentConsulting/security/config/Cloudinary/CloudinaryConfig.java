package studentConsulting.security.config.Cloudinary;

import com.cloudinary.Cloudinary;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class CloudinaryConfig {

    @Bean
    public Cloudinary getCloudinary() {
        Map<String, String> config = new HashMap<>();
        config.put("cloud_name", "dobxsmmr8");
        config.put("api_key", "755633741691519");
        config.put("api_secret", "yDNAsBc-ykHRAFXCnVbydbSpU6M");
        config.put("secure", "true");
        return new Cloudinary(config);
    }
}
