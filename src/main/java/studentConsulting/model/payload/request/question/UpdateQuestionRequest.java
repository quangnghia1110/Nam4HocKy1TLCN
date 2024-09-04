package studentConsulting.model.payload.request.question;

import org.springframework.web.multipart.MultipartFile;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UpdateQuestionRequest {
    private Integer questionId;
	private Integer departmentId;
    private Integer fieldId;
    private Integer roleAskId;
    private String title;
    private String content;
    private String firstName;
    private String lastName;
    private String studentCode;
    private Boolean statusPublic;
    private MultipartFile file; 
}
