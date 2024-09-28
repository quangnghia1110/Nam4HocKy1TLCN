package studentConsulting.service.implement.common;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Map;

@Service
public class CommonFileStorageServiceImpl {

    private final Cloudinary cloudinary;
    private final studentConsulting.security.config.Github.GitHubService gitHubService;  // Sử dụng GitHubService thay vì DropboxService

    @Autowired
    public CommonFileStorageServiceImpl(Cloudinary cloudinary, studentConsulting.security.config.Github.GitHubService gitHubService) {
        this.cloudinary = cloudinary;
        this.gitHubService = gitHubService;
    }

    public String saveFile(MultipartFile file) {
        try {
            String fileType = file.getContentType();

            if (fileType != null && fileType.startsWith("image")) {
                Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
                return (String) uploadResult.get("url");
            } else {
                return gitHubService.uploadFile(file);
            }
        } catch (IOException e) {
            throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
        }
    }
}
