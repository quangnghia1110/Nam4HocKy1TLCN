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
    private String answerContent;
    private String answerTitle;
    private Integer departmentId;
    private Boolean status;
}

