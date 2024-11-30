package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

import java.time.LocalDate;

@Data
@Builder
public class CommonQuestionRequest {

    private String title;
    private String content;
    private LocalDate createdAt;
    private int departmentId;
    private int fieldId;
    private int roleAskId;
    private String answerContent;
    private String answerTitle;
    private LocalDate answerCreatedAt;
    private String answerEmail;
    private String answerUserFirstname;
    private String answerUserLastname;
    private String askerFirstname;
    private String askerLastname;
    private Boolean status;
}

