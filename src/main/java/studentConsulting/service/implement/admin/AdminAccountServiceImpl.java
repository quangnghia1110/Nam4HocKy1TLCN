package studentConsulting.service.implement.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;
import studentConsulting.model.payload.mapper.admin.AccountMapper;
import studentConsulting.repository.admin.AccountRepository;
import studentConsulting.repository.admin.DepartmentRepository;
import studentConsulting.repository.admin.RoleConsultantRepository;
import studentConsulting.repository.admin.RoleRepository;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.specification.admin.AccountSpecification;

import java.time.LocalDate;
import java.util.Optional;

@Service
public class AdminAccountServiceImpl implements IAdminAccountService {

    @Autowired
    private AccountRepository accountRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private RoleRepository roleRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private AccountMapper accountMapper;

    @Override
    public Page<ManageAccountDTO> getAccountByAdmin(String email, String username, Boolean isOnline, Optional<LocalDate> startDate, Optional<LocalDate> endDate, Boolean isActivity, Pageable pageable) {
        Specification<AccountEntity> spec = Specification.where(null);

        if (email != null && !email.isEmpty()) {
            spec = spec.and(AccountSpecification.hasEmail(email));
        }

        if (username != null && !username.isEmpty()) {
            spec = spec.and(AccountSpecification.hasUsername(username));
        }

        if (isOnline != null) {
            spec = spec.and(AccountSpecification.isOnline(isOnline));
        }

        if (isActivity != null) {
            spec = spec.and(AccountSpecification.isActive(isActivity));
        }

        if (startDate.isPresent() && endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactDateRange(startDate.get(), endDate.get()));
        } else if (startDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasExactStartDate(startDate.get()));
        } else if (endDate.isPresent()) {
            spec = spec.and(AccountSpecification.hasDateBefore(endDate.get()));
        }

        Page<AccountEntity> accountEntities = accountRepository.findAll(spec, pageable);
        return accountEntities.map(accountMapper::mapToDTO);
    }


    @Override
    public ManageAccountDTO getAccountById(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));
        return accountMapper.mapToDTO(accountEntity);
    }

    @Override
    public ManageAccountDTO changeAccountActivity(Integer id) {
        AccountEntity accountEntity = accountRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy tài khoản với ID: " + id));

        accountEntity.setActivity(!accountEntity.isActivity());
        AccountEntity updatedAccount = accountRepository.save(accountEntity);

        return accountMapper.mapToDTO(updatedAccount);
    }
}
