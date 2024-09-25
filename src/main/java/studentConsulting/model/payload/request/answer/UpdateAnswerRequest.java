package studentConsulting.model.payload.request.answer;

import org.springframework.web.multipart.MultipartFile;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UpdateAnswerRequest {
    private String title;
    private String content;
    private Boolean statusApproval;
    private Boolean statusAnswer;
    private MultipartFile file;
}

