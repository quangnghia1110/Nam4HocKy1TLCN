package studentConsulting.service.implement.common;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.Map;

@Service
public class CommonFileStorageServiceImpl {

    private final Cloudinary cloudinary;

    @Autowired
    public CommonFileStorageServiceImpl(Cloudinary cloudinary) {
        this.cloudinary = cloudinary;
    }

    public String saveFile(MultipartFile file) {
        try {
            Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of("resource_type", "auto"));
            String cloudinaryUrl = (String) uploadResult.get("url");

            saveFileToLocal(file);

            return cloudinaryUrl;
        } catch (IOException e) {
            System.out.println("Error during file upload: " + e.getMessage());
            throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
        }
    }


    private void saveFileToLocal(MultipartFile file) throws IOException {
        String localFilePath = Paths.get(FilePaths.UPLOAD_FILE, file.getOriginalFilename()).toString();

        File directory = new File(FilePaths.UPLOAD_FILE);
        if (!directory.exists()) {
            directory.mkdirs();
        }

        try (FileOutputStream fos = new FileOutputStream(localFilePath)) {
            fos.write(file.getBytes());
        }
    }
}
