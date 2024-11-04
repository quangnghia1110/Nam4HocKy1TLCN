package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

@Builder
@Data
public class CreateQuestionRequest {
    private Integer departmentId;
    private Integer fieldId;
    private Integer roleAskId;
    private String title;
    private String content;
    private String firstName;
    private String lastName;
    private Integer views;
    private Boolean statusPublic;
    private MultipartFile file;
    private Boolean statusApproval;
}

