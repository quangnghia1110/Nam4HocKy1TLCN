package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageUserDTO;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface IAdminUserInformationService {
    Page<ManageUserDTO> getAllUsersWithFilters(String name, String studentCode, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Pageable pageable);

    ManageUserDTO getUserById(Integer id);

    void importUsers(List<List<String>> csvData);
}
