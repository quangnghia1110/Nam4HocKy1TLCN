package studentConsulting.model.payload.request.question_answer;

import org.springframework.web.multipart.MultipartFile;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class UpdateCommonQuestionRequest {
    private String title;
    private String content;
    private MultipartFile fileName; 

    private String answerTitle;
    private String answerContent;
}

