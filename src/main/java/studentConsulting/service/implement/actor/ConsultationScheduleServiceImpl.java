package studentConsulting.service.implement.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleEntity;
import studentConsulting.model.entity.consultation_schedule.ConsultationScheduleRegistrationEntity;
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.mapper.actor.ConsultationScheduleMapper;
import studentConsulting.model.payload.request.consultant.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.actor.IConsultationScheduleService;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleRegistrationSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class ConsultationScheduleServiceImpl implements IConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConsultationScheduleRegistrationRepository consultationScheduleRegistrationRepository;

    @Autowired
    private ConsultationScheduleMapper consultationScheduleMapper;

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
        schedule.setCreatedAt(LocalDate.now());

        ConsultationScheduleEntity savedSchedule = consultationScheduleRepository.save(schedule);

        return consultationScheduleMapper.mapToDTO(savedSchedule);
    }

    @Override
    public ManageConsultantScheduleDTO confirmConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request) {

        ConsultationScheduleEntity existingSchedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));


        if (request.getStatusConfirmed()) {
            if (existingSchedule.getMode() != null && existingSchedule.getMode()) {
                if (request.getLocation() != null) {
                    throw new ErrorException("Không được phép nhập địa điểm cho tư vấn online.");
                }
                if (request.getLink() == null || request.getConsultationDate() == null || request.getConsultationTime() == null) {
                    throw new ErrorException("Phải cung cấp đầy đủ thông tin link, ngày và giờ cho tư vấn online.");
                }
                existingSchedule.setLink(request.getLink());
            } else { // Offline
                if (request.getLink() != null) {
                    throw new ErrorException("Không được phép nhập link cho tư vấn offline.");
                }
                if (request.getLocation() == null || request.getConsultationDate() == null || request.getConsultationTime() == null) {
                    throw new ErrorException("Phải cung cấp đầy đủ thông tin địa điểm, ngày và giờ cho tư vấn offline.");
                }
                existingSchedule.setLocation(request.getLocation());
            }
            existingSchedule.setStatusConfirmed(true);
            existingSchedule.setConsultationDate(request.getConsultationDate());
            existingSchedule.setConsultationTime(request.getConsultationTime());
        } else {
            existingSchedule.setStatusConfirmed(false);
        }

        existingSchedule.setTitle(request.getTitle());
        existingSchedule.setContent(request.getContent());
        existingSchedule.setConsultationDate(request.getConsultationDate());
        existingSchedule.setConsultationTime(request.getConsultationTime());
        existingSchedule.setLocation(request.getLocation());
        existingSchedule.setLink(request.getLink());
        existingSchedule.setMode(request.getMode());
        existingSchedule.setStatusPublic(request.getStatusPublic());
        existingSchedule.setStatusConfirmed(request.getStatusConfirmed());

        ConsultationScheduleEntity updatedSchedule = consultationScheduleRepository.save(existingSchedule);

        return consultationScheduleMapper.mapToDTOs(updatedSchedule);
    }

    @Override
    public ConsultationScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId, Integer userId) {

        ConsultationScheduleEntity newSchedule = new ConsultationScheduleEntity();
        DepartmentEntity department = departmentRepository.findById(departmentId)
                .orElseThrow(() -> new ErrorException("Phòng ban không tồn tại"));

        newSchedule.setTitle(request.getTitle());
        newSchedule.setContent(request.getContent());
        newSchedule.setConsultationDate(request.getConsultationDate());
        newSchedule.setConsultationTime(request.getConsultationTime());
        newSchedule.setLocation(request.getLocation());
        newSchedule.setLink(request.getLink());
        newSchedule.setMode(request.getMode());
        newSchedule.setStatusPublic(true);
        newSchedule.setStatusConfirmed(true);
        newSchedule.setDepartment(department);
        newSchedule.setCreatedBy(userId);

        ConsultationScheduleEntity savedSchedule = consultationScheduleRepository.save(newSchedule);
        return consultationScheduleMapper.mapToDTO(savedSchedule);
    }

    @Override
    public Page<ConsultationScheduleDTO> getConsultationScheduleByRole(UserInformationEntity user, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable) {
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        Specification<ConsultationScheduleEntity> spec = Specification.where(null);

        if (SecurityConstants.Role.USER.equals(role)) {
            spec = spec.and(ConsultationScheduleSpecification.hasUser(user));
        }

        if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            if (departmentId != null) {
                spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
            }
        }

        if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            if (departmentId != null) {
                spec = spec.and(ConsultationScheduleSpecification.hasDepartment(departmentId));
                spec = spec.or(ConsultationScheduleSpecification.isCreatedByAdvisor());
            }
        }

        if (title != null && !title.trim().isEmpty()) {
            spec = spec.and(ConsultationScheduleSpecification.hasTitle(title));
        }
        if (statusPublic != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusPublic(statusPublic));
        }
        if (statusConfirmed != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusConfirmed(statusConfirmed));
        }
        if (mode != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasMode(mode));
        }
        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        return consultationScheduleRepository.findAll(spec, pageable).map(consultationScheduleMapper::mapToDTO);
    }

    @Override
    public ConsultationScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, boolean isAdmin, UpdateConsultationScheduleRequest request, String role, Integer userId) {
        Optional<ConsultationScheduleEntity> scheduleOpt;

        if (SecurityConstants.Role.ADMIN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findById(scheduleId);
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentOrCreatedBy(scheduleId, departmentId, userId);
        } else if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentId(scheduleId, departmentId);
        } else {
            scheduleOpt = consultationScheduleRepository.findByIdAndCreatedBy(scheduleId, userId);
        }

        ConsultationScheduleEntity existingSchedule = scheduleOpt.orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại hoặc bạn không có quyền cập nhật lịch này"));

        existingSchedule.setTitle(request.getTitle());
        existingSchedule.setContent(request.getContent());
        existingSchedule.setConsultationDate(request.getConsultationDate());
        existingSchedule.setConsultationTime(request.getConsultationTime());
        existingSchedule.setLocation(request.getLocation());
        existingSchedule.setLink(request.getLink());
        existingSchedule.setMode(request.getMode());
        existingSchedule.setStatusPublic(request.getStatusPublic());
        existingSchedule.setStatusConfirmed(request.getStatusConfirmed());

        ConsultationScheduleEntity updatedSchedule = consultationScheduleRepository.save(existingSchedule);
        return consultationScheduleMapper.mapToDTO(updatedSchedule);
    }

    @Override
    public void deleteConsultationSchedule(Integer scheduleId, Integer departmentId, Integer userId, String role) {
        Optional<ConsultationScheduleEntity> scheduleOpt;

        if (SecurityConstants.Role.ADMIN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findById(scheduleId);
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentOrCreatedBy(scheduleId, departmentId, userId);
        } else if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentId(scheduleId, departmentId);
        } else {
            scheduleOpt = consultationScheduleRepository.findByIdAndCreatedBy(scheduleId, userId);
        }

        if (!scheduleOpt.isPresent()) {
            throw new ErrorException("Lịch tư vấn không tồn tại hoặc bạn không có quyền xóa lịch này");
        }

        consultationScheduleRepository.delete(scheduleOpt.get());
    }

    @Override
    public ConsultationScheduleDTO getConsultationScheduleByRole(Integer scheduleId, String role, Integer departmentId, Integer userId) {
        Optional<ConsultationScheduleEntity> scheduleOpt;

        if (SecurityConstants.Role.ADMIN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findById(scheduleId);
        } else if (SecurityConstants.Role.TRUONGBANTUVAN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentOrCreatedBy(scheduleId, departmentId, userId);
        } else if (SecurityConstants.Role.TUVANVIEN.equals(role)) {
            scheduleOpt = consultationScheduleRepository.findByIdAndDepartmentId(scheduleId, departmentId);
        } else {
            scheduleOpt = consultationScheduleRepository.findByIdAndCreatedBy(scheduleId, userId);
        }

        ConsultationScheduleEntity schedule = scheduleOpt.orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        return consultationScheduleMapper.mapToDTO(schedule);
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
    public Page<ConsultationScheduleRegistrationMemberDTO> getMembersByConsultationSchedule(
            Integer consultationScheduleId, LocalDate startDate, LocalDate endDate, Pageable pageable, Integer userId) {

        consultationScheduleRepository.findById(consultationScheduleId)
                .orElseThrow(() -> new ErrorException("Buổi tư vấn không tồn tại."));

        Specification<ConsultationScheduleRegistrationEntity> spec = Specification
                .where(ConsultationScheduleRegistrationSpecification.hasConsultationSchedule(consultationScheduleId))
                .and(ConsultationScheduleRegistrationSpecification.hasStatus(true));

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasStartDateAfterOrEqual(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleRegistrationSpecification.hasEndDateBeforeOrEqual(endDate));
        }

        Page<ConsultationScheduleRegistrationEntity> registrations = consultationScheduleRegistrationRepository.findAll(spec, pageable);

        return registrations.map(registration -> {
            String userName = registration.getUser().getLastName() + " " + registration.getUser().getFirstName();
            return ConsultationScheduleRegistrationMemberDTO.builder()
                    .userName(userName)
                    .registeredAt(registration.getRegisteredAt())
                    .status(registration.getStatus())
                    .build();
        });
    }

    @Override
    public void importManageConsultantSchedules(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream().skip(1).collect(Collectors.toList());

        List<ManageConsultantScheduleDTO> consultantSchedules = filteredData.stream().map(row -> {
            try {
                Integer id = Integer.parseInt(row.get(0));
                String title = row.get(1);
                String content = row.get(2);
                LocalDate consultationDate = LocalDate.parse(row.get(3));
                String consultationTime = row.get(4);
                String location = row.get(5);
                String link = row.get(6);
                Boolean mode = Boolean.parseBoolean(row.get(7));
                Boolean statusPublic = Boolean.parseBoolean(row.get(8));
                Boolean statusConfirmed = Boolean.parseBoolean(row.get(9));
                Integer createdById = Integer.parseInt(row.get(10));

                return ManageConsultantScheduleDTO.builder()
                        .id(id)
                        .title(title)
                        .content(content)
                        .consultationDate(consultationDate)
                        .consultationTime(consultationTime)
                        .location(location)
                        .link(link)
                        .mode(mode)
                        .statusPublic(statusPublic)
                        .statusConfirmed(statusConfirmed)
                        .created_by(createdById)
                        .build();
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Consultation Schedule: " + e.getMessage());
            }
        }).collect(Collectors.toList());

        consultantSchedules.forEach(schedule -> {
            try {
                ConsultationScheduleEntity entity = new ConsultationScheduleEntity();
                entity.setId(schedule.getId());
                entity.setTitle(schedule.getTitle());
                entity.setContent(schedule.getContent());
                entity.setConsultationDate(schedule.getConsultationDate());
                entity.setConsultationTime(schedule.getConsultationTime());
                entity.setLocation(schedule.getLocation());
                entity.setLink(schedule.getLink());
                entity.setMode(schedule.getMode());
                entity.setStatusPublic(schedule.getStatusPublic());
                entity.setStatusConfirmed(schedule.getStatusConfirmed());

                UserInformationEntity createdBy = userRepository.findById(schedule.getCreated_by())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người tạo với ID: " + schedule.getCreated_by()));

                entity.setCreatedBy(createdBy.getId());

                consultationScheduleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Consultation Schedule vào database: " + e.getMessage());
            }
        });
    }

    //check lai
    @Override
    public Page<ConsultationScheduleDTO> getAllConsultationSchedulesWithFilters(
            String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConsultationScheduleEntity> spec = Specification.where(null);

        if (title != null && !title.isEmpty()) {
            spec = spec.and(ConsultationScheduleSpecification.hasTitle(title));
        }

        if (statusPublic != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusPublic(statusPublic));
        }

        if (statusConfirmed != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasStatusConfirmed(statusConfirmed));
        }

        if (mode != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasMode(mode));
        }

        if (startDate != null && endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactDateRange(startDate, endDate));
        } else if (startDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasExactStartDate(startDate));
        } else if (endDate != null) {
            spec = spec.and(ConsultationScheduleSpecification.hasDateBefore(endDate));
        }

        Page<ConsultationScheduleEntity> schedules = consultationScheduleRepository.findAll(spec, pageable);
        return schedules.map(consultationScheduleMapper::mapToDTO);
    }
}
