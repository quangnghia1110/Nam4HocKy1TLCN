package studentConsulting.service.implement.admin;

import com.cloudinary.provisioning.Account;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.RoleConsultantEntity;
import studentConsulting.model.entity.RoleEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;
import studentConsulting.model.payload.dto.manage.UpdateAccountDTO;
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

    @Autowired
    PasswordEncoder passwordEncoder;

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
    public ManageAccountDTO updateAccount(Integer id, UpdateAccountDTO accountRequest) {
        AccountEntity account = accountRepository.findById(id)
                .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy tài khoản với ID: " + id));

        if (accountRequest.getActivity() != null) {
            account.setActivity(accountRequest.getActivity());
        }

        if (accountRequest.getRoleId() != null) {
            RoleEntity role = roleRepository.findById(accountRequest.getRoleId())
                    .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy vai trò với ID: " + accountRequest.getRoleId()));
            account.setRole(role);
        }

        if (accountRequest.getRoleConsultantId() != null) {
            RoleConsultantEntity roleConsultant = roleConsultantRepository.findById(accountRequest.getRoleConsultantId())
                    .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy vai trò tư vấn với ID: " + accountRequest.getRoleConsultantId()));
            account.setRoleConsultant(roleConsultant);
        }

        if (accountRequest.getUsername() != null && !accountRequest.getUsername().equals(account.getUsername())) {
            if (accountRepository.existsByUsername(accountRequest.getUsername())) {
                throw new Exceptions.ErrorException("Tên người dùng đã tồn tại: " + accountRequest.getUsername());
            }
            account.setUsername(accountRequest.getUsername());
        }

        if (accountRequest.getEmail() != null && !accountRequest.getEmail().equals(account.getEmail())) {
            if (accountRepository.existsByEmail(accountRequest.getEmail())) {
                throw new Exceptions.ErrorException("Email đã tồn tại: " + accountRequest.getEmail());
            }
            account.setEmail(accountRequest.getEmail());
        }

        if (accountRequest.getPassword() != null && !accountRequest.getPassword().isBlank()) {
            account.setPassword(passwordEncoder.encode(accountRequest.getPassword()));
        }

        AccountEntity updatedAccount = accountRepository.save(account);

        return accountMapper.mapToDTO(updatedAccount);
    }



}
