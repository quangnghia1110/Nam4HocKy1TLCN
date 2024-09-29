package studentConsulting.model.payload.request.question_answer;

import org.springframework.web.multipart.MultipartFile;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ReviewAnswerRequest {

    private Integer questionId; // ID của câu hỏi cần kiểm duyệt
    private String content;     // Nội dung mới sau khi kiểm duyệt
    private MultipartFile file; // Tệp tin mới (nếu có) kèm theo câu trả lời sau khi kiểm duyệt
}
