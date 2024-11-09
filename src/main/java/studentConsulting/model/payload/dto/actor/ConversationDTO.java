package studentConsulting.model.payload.dto.actor;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ConversationDTO {
    private Integer id;
    private DepartmentDTO department;
    private String name;
    private Boolean isGroup;
    private LocalDate createdAt;
    private String avatarUrl;
    private List<MemberDTO> members;
}
