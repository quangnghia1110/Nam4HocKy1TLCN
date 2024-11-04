package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;
import org.springframework.web.multipart.MultipartFile;

@Data
@Builder
public class UpdateCommonQuestionRequest {
    private String title;
    private String content;
    private MultipartFile fileName;

    private String answerTitle;
    private String answerContent;
}

