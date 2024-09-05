package studentConsulting.model.payload.request.question;

import java.time.LocalDateTime;

import org.springframework.web.multipart.MultipartFile;
import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class CreateFollowUpQuestionRequest {
	private Integer parentQuestionId;
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
    private Boolean statusApproval;
}

