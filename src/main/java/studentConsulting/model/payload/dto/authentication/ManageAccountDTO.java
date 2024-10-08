package studentConsulting.model.payload.dto.authentication;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDate;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ManageAccountDTO {
    private Integer id;
    private LocalDate createdAt;
    private Boolean isActivity;
    private String username;
    private String email;
    private DepartmentDTO department;
    private RoleDTO role;
    private RoleConsultantDTO roleConsultant;
    private LocalDateTime lastActivity;
    private Boolean isOnline;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class DepartmentDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleDTO {
        private Integer id;
        private String name;
    }

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class RoleConsultantDTO {
        private Integer id;
        private String name;
    }
}
