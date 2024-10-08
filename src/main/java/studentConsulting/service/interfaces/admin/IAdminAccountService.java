package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface IAdminAccountService {

    Page<ManageAccountDTO> getAllAccountsWithFilters(Optional<String> email, Optional<String> username, Optional<Boolean> isOnline, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Optional<Boolean> isActivity, Pageable pageable);

    ManageAccountDTO getAccountById(Integer id);

    ManageAccountDTO changeAccountActivity(Integer id);

    void importAccounts(List<List<String>> csvData);


}
