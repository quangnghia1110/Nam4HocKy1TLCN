package studentConsulting.model.payload.request;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.web.multipart.MultipartFile;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class UpdatePostRequest {
    private String title;
    private String content;
    private boolean isAnonymous;
    private MultipartFile file;
}
