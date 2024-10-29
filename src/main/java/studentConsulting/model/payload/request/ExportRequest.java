package studentConsulting.model.payload.request;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDate;
import java.util.Optional;

@Builder
@Data
public class ExportRequest {

    private String dataType;
    private String exportType;
    private int page = 0;
    private int size = 10;
    private String sortBy = "createdAt";
    private String sortDir = "asc";
    private LocalDate startDate;
    private LocalDate endDate;
    private Integer conversationId; // cần cho message
    private Optional<String> departId; // cần cho field
    private String title;
    private Integer toDepartmentId;
    private String status;
    private String name;
    private String email;
    private String username;
    private Optional<Boolean> isOnline;
    private Optional<Boolean> isActivity;
    private Integer id;
    private String line;
    private String provinceCode;
    private String districtCode;
    private String wardCode;
    private Optional<String> code;
    private Optional<String> nameEn;
    private Optional<String> fullName;
    private Optional<String> fullNameEn;
    private Optional<String> codeName;
    private Integer roleId;
    private String studentCode;
}
