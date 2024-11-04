package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface IAdminAccountService {

    Page<ManageAccountDTO> getAllAccountsWithFilters(String email, String username, Boolean isOnline, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Boolean isActivity, Pageable pageable);

    ManageAccountDTO getAccountById(Integer id);

    ManageAccountDTO changeAccountActivity(Integer id);

    void importAccounts(List<List<String>> csvData);


}
