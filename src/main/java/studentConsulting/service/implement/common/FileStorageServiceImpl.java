package studentConsulting.service.implement.common;

import com.cloudinary.Cloudinary;
import com.cloudinary.utils.ObjectUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.FilePaths;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

@Service
public class FileStorageServiceImpl {

    private final Cloudinary cloudinary;

    @Autowired
    public FileStorageServiceImpl(Cloudinary cloudinary) {
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

    public void deleteFile(String fileUrl) {
        String publicId = getPublicIdFromUrl(fileUrl);

        try {
            cloudinary.uploader().destroy(publicId, ObjectUtils.emptyMap());
            System.out.println("File deleted from Cloudinary: " + fileUrl);
        } catch (Exception e) {
            System.err.println("Error deleting file from Cloudinary: " + e.getMessage());
        }

        try {
            Path localFilePath = Paths.get(FilePaths.UPLOAD_FILE, Paths.get(fileUrl).getFileName().toString());
            Files.deleteIfExists(localFilePath);
        } catch (IOException e) {
            System.err.println("Error deleting file from local storage: " + e.getMessage());
        }
    }

    private String getPublicIdFromUrl(String fileUrl) {
        String[] parts = fileUrl.split("/");
        String fileNameWithExtension = parts[parts.length - 1];
        return fileNameWithExtension.contains(".")
                ? fileNameWithExtension.substring(0, fileNameWithExtension.lastIndexOf('.'))
                : fileNameWithExtension;
    }
}
