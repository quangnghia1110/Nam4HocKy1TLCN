package studentConsulting.service.implement.user;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.repository.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.repository.common.DepartmentRepository;
import studentConsulting.service.interfaces.user.IUserConsultationScheduleService;
import studentConsulting.specification.ConsultationScheduleRegistrationSpecification;
import studentConsulting.specification.ConsultationScheduleSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

@Service
public class UserConsultationScheduleServiceImpl implements IUserConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConsultationScheduleRegistrationRepository consultationScheduleRegistrationRepository;

    @Override
    public ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request, UserInformationEntity user) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        Optional<UserInformationEntity> consultantOpt = userRepository.findById(request.getConsultantId());
        if (!consultantOpt.isPresent()) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không tồn tại"));
        }

        Optional<DepartmentEntity> departmentOpt = departmentRepository.findById(request.getDepartmentId());
        if (!departmentOpt.isPresent()) {
            errors.add(new FieldErrorDetail("department", "Phòng ban không tồn tại"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        UserInformationEntity consultant = consultantOpt.get();
        DepartmentEntity department = departmentOpt.get();

        if (!consultant.getAccount().getDepartment().getId().equals(department.getId())) {
            errors.add(new FieldErrorDetail("consultant", "Tư vấn viên không thuộc phòng ban đã chọn"));
        }

        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }
        ConsultationScheduleEntity schedule = new ConsultationScheduleEntity();
        schedule.setUser(user);
        schedule.setConsultant(consultant);
        schedule.setDepartment(department);
        schedule.setTitle(request.getTitle());
        schedule.setContent(request.getContent());
        schedule.setMode(request.getMode());
        schedule.setStatusPublic(request.getStatusPublic());
        schedule.setCreatedBy(user.getId());

        ConsultationScheduleEntity savedSchedule = consultationScheduleRepository.save(schedule);

        return mapToDTO(savedSchedule);
    }

    @Override
    public Page<ConsultationScheduleDTO> getSchedulesByUserWithFilters(UserInformationEntity user, Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasUser(user));

        if (departmentId != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
        }

        if (title != null && !title.trim().isEmpty()) {
            spec = spec.and(ConsultationScheduleSpecification.hasTitle(title));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        return consultationScheduleRepository.findAll(spec, pageable).map(this::mapToDTO);
    }

    @Override
    public ConsultationScheduleRegistrationDTO registerForConsultation(ConsultationScheduleRegistrationRequest request, UserInformationEntity user) {
        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(request.getConsultationScheduleId())
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        boolean isAlreadyRegistered = consultationScheduleRegistrationRepository.existsByUserAndConsultationSchedule(user, schedule);

        if (isAlreadyRegistered) {
            throw new ErrorException("Bạn đã đăng ký tham gia lịch tư vấn này trước đó.");
        }

        ConsultationScheduleRegistrationEntity registration = ConsultationScheduleRegistrationEntity.builder()
                .user(user)
                .consultationSchedule(schedule)
                .registeredAt(LocalDate.now())
                .status(true)
                .build();

        registration = consultationScheduleRegistrationRepository.save(registration);

        ConsultationScheduleRegistrationDTO.UserDTO userDTO = ConsultationScheduleRegistrationDTO.UserDTO.builder()
                .id(user.getId())
                .name(user.getLastName() + " " + user.getFirstName())
                .build();

        ConsultationScheduleRegistrationDTO.ConsultationScheduleDTO consultationScheduleDTO = ConsultationScheduleRegistrationDTO.ConsultationScheduleDTO.builder()
                .id(schedule.getId())
                .title(schedule.getTitle())
                .build();

        return ConsultationScheduleRegistrationDTO.builder()
                .id(registration.getId())
                .user(userDTO)
                .consultationSchedule(consultationScheduleDTO)
                .registeredAt(registration.getRegisteredAt())
                .status(registration.getStatus())
                .build();
    }

    @Override
    public void cancelRegistrationForConsultation(Integer id, UserInformationEntity user) {
        ConsultationScheduleRegistrationEntity registration = consultationScheduleRegistrationRepository.findById(id)
                .orElseThrow(() -> new ErrorException("Không tìm thấy bản ghi đăng ký này."));

        if (!registration.getUser().equals(user)) {
            throw new ErrorException("Bạn không có quyền hủy đăng ký này.");
        }

        consultationScheduleRegistrationRepository.delete(registration);
    }

    @Override
    public Page<ConsultationScheduleRegistrationDTO> getSchedulesJoinByUser(UserInformationEntity user, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConsultationScheduleRegistrationEntity> spec = Specification
                .where(ConsultationScheduleRegistrationSpecification.hasUser(user))
                .and(ConsultationScheduleRegistrationSpecification.hasStatus(true));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasStartDateAfterOrEqual(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasEndDateBeforeOrEqual(endDate));
        }

        Page<ConsultationScheduleRegistrationEntity> registrationEntities = consultationScheduleRegistrationRepository.findAll(spec, pageable);

        return registrationEntities.map(registration -> {
            ConsultationScheduleRegistrationDTO.UserDTO userDTO = ConsultationScheduleRegistrationDTO.UserDTO.builder()
                    .id(registration.getUser().getId())
                    .name(registration.getUser().getLastName() + " " + registration.getUser().getFirstName())
                    .build();

            ConsultationScheduleRegistrationDTO.ConsultationScheduleDTO consultationScheduleDTO = ConsultationScheduleRegistrationDTO.ConsultationScheduleDTO.builder()
                    .id(registration.getConsultationSchedule().getId())
                    .title(registration.getConsultationSchedule().getTitle())
                    .build();

            return ConsultationScheduleRegistrationDTO.builder()
                    .id(registration.getId())
                    .user(userDTO)
                    .consultationSchedule(consultationScheduleDTO)
                    .registeredAt(registration.getRegisteredAt())
                    .status(registration.getStatus())
                    .build();
        });
    }


    private ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
                .id(schedule.getId())
                .department(schedule.getDepartment() != null
                        ? new DepartmentDTO(
                        schedule.getDepartment().getId(),
                        schedule.getDepartment().getName()
                )
                        : null)
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .consultantName(schedule.getConsultant().getLastName() + " " + schedule.getConsultant().getFirstName())
                .userName(schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName())
                .createdBy(schedule.getUser().getId())
                .build();
    }
}
