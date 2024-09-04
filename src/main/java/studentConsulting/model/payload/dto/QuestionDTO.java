package studentConsulting.model.payload.dto;

import org.springframework.web.multipart.MultipartFile;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class QuestionDTO {
    private Integer departmentId;
    private Integer fieldId;
    private Integer roleAskId;
    private String title;
    private String content;
    private String firstName;
    private String lastName;
    private String studentCode;
    private Boolean statusPublic;
    private String fileName; 
    private Boolean statusApproval;
}
