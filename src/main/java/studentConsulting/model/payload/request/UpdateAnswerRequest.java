package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

@Data
@Builder
public class UpdateAnswerRequest {
    private String title;
    private String content;
    private Boolean statusApproval;
    private MultipartFile file;
}

