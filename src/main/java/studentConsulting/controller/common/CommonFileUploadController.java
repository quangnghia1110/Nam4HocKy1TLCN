package studentConsulting.controller.common;

import com.cloudinary.Cloudinary;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.service.implement.common.CommonFileStorageServiceImpl;

@RestController
@RequestMapping("${base.url}")
public class CommonFileUploadController {

    @Autowired
    private Cloudinary cloudinary;

    @Autowired
    private CommonFileStorageServiceImpl fileStorageService;

    @PostMapping("/upload")
    public ResponseEntity<FileUploadResponse> uploadFile(@RequestParam("file") MultipartFile file) {
        try {
            String fileUrl = fileStorageService.saveFile(file);

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
}

