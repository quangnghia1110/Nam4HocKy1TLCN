package studentConsulting.model.payload.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CreateAnswerRequest {
    private Integer questionId;
    private Integer roleConsultantId;
    private Integer consultantId;
    private String title;
    private String content;
    private MultipartFile file;
    private Boolean statusApproval;
}
