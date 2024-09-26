package studentConsulting.model.payload.dto;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;

@Data
@Builder
public class QuestionDTO {
    private Integer id;

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
    private Integer views;
    private Boolean statusApproval;
    private LocalDate createdAt;
}
