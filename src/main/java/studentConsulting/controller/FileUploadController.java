package studentConsulting.controller;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

@RestController
@RequestMapping("${base.url}")
public class FileUploadController {
   
	@Autowired
    private Cloudinary cloudinary;

    @PostMapping("/upload")
    public ResponseEntity<FileUploadResponse> uploadFile(@RequestParam("file") MultipartFile file) {
        try {
            String fileUrl = saveFile(file);

            FileUploadResponse response = new FileUploadResponse(fileUrl);
            return new ResponseEntity<>(response, HttpStatus.OK);
        } catch (RuntimeException e) {
            return new ResponseEntity<>(null, HttpStatus.INTERNAL_SERVER_ERROR);
        }
    }

    public static class FileUploadResponse {
        private String fileUrl;

        public FileUploadResponse(String fileUrl) {
            this.fileUrl = fileUrl;
        }

        public String getFileUrl() {
            return fileUrl;
        }

        public void setFileUrl(String fileUrl) {
            this.fileUrl = fileUrl;
        }
    }
    
    private String saveFile(MultipartFile file) {
	    try {
	        String fileType = file.getContentType();

	        if (fileType != null && fileType.startsWith("image")) {
	            Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
	            String fileUrl = (String) uploadResult.get("url");
	            return fileUrl; 
	        } else {
	            String uploadDir = "D:\\HCMUTE-K21\\DoAnGitHub\\Nam4HocKy1TLCN\\upload";
	            Path uploadPath = Paths.get(uploadDir);

	            if (!Files.exists(uploadPath)) {
	                Files.createDirectories(uploadPath);
	            }

	            String originalFileName = file.getOriginalFilename();
	            Path filePath = uploadPath.resolve(originalFileName);

	            Files.write(filePath, file.getBytes());

	            return filePath.toString();  
	        }
	    } catch (IOException e) {
	        throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
	    }
	}
}

