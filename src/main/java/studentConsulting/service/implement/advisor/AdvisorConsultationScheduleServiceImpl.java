package studentConsulting.service.implement.advisor;

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
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.dto.department_field.DepartmentDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRegistrationRepository;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.department_field.DepartmentRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConsultationScheduleService;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleRegistrationSpecification;
import studentConsulting.specification.consultation_schedule.ConsultationScheduleSpecification;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AdvisorConsultationScheduleServiceImpl implements IAdvisorConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

    @Autowired
    private ConsultationScheduleRegistrationRepository consultationScheduleRegistrationRepository;

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
        return schedules.map(this::mapToDTO);
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
        return mapToDTO(savedSchedule);
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

        return mapToDTO(schedule);
    }

    private ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
                .id(schedule.getId())
                .department(schedule.getDepartment() != null
                        ? new DepartmentDTO(schedule.getDepartment().getId(), schedule.getDepartment().getName())
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
                .consultantName(schedule.getConsultant() != null
                        ? schedule.getConsultant().getLastName() + " " + schedule.getConsultant().getFirstName()
                        : null)
                .userName(schedule.getUser() != null
                        ? schedule.getUser().getLastName() + " " + schedule.getUser().getFirstName()
                        : null)
                .build();
    }

    private ManageConsultantScheduleDTO mapToDTOs(ConsultationScheduleEntity schedule) {
        return ManageConsultantScheduleDTO.builder()
                .id(schedule.getId())
                .title(schedule.getTitle())
                .content(schedule.getContent())
                .consultationDate(schedule.getConsultationDate())
                .consultationTime(schedule.getConsultationTime())
                .location(schedule.getLocation())
                .link(schedule.getLink())
                .mode(schedule.getMode())
                .statusPublic(schedule.getStatusPublic())
                .statusConfirmed(schedule.getStatusConfirmed())
                .created_by(schedule.getCreatedBy())
                .build();
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
        return mapToDTO(updatedSchedule);
    }

    @Override
    public Page<ManageConsultantScheduleDTO> getAllConsultationsWithFilters(String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable) {

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
        return schedules.map(this::mapToDTOs);
    }

    @Override
    public void importConsultationSchedules(List<List<String>> csvData) {
        List<List<String>> filteredData = csvData.stream().skip(1).collect(Collectors.toList());

        List<ConsultationScheduleDTO> consultationSchedules = filteredData.stream().map(row -> {
            try {
                Integer id = Integer.parseInt(row.get(0));
                String title = row.get(1);
                String content = row.get(2);
                String consultantName = row.get(3);
                LocalDate consultationDate = LocalDate.parse(row.get(4));
                String consultationTime = row.get(5);
                String location = row.get(6);
                String link = row.get(7);
                Boolean mode = Boolean.parseBoolean(row.get(8));
                Boolean statusPublic = Boolean.parseBoolean(row.get(9));
                Boolean statusConfirmed = Boolean.parseBoolean(row.get(10));
                Integer departmentId = Integer.parseInt(row.get(11));
                Integer createdById = Integer.parseInt(row.get(12));

                return ConsultationScheduleDTO.builder()
                        .id(id)
                        .title(title)
                        .content(content)
                        .consultantName(consultantName)
                        .consultationDate(consultationDate)
                        .consultationTime(consultationTime)
                        .location(location)
                        .link(link)
                        .mode(mode)
                        .statusPublic(statusPublic)
                        .statusConfirmed(statusConfirmed)
                        .department(new DepartmentDTO(departmentId, null))
                        .createdBy(createdById)
                        .build();
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi parse dữ liệu Consultation Schedule: " + e.getMessage());
            }
        }).collect(Collectors.toList());

        consultationSchedules.forEach(schedule -> {
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

                DepartmentEntity department = departmentRepository.findById(schedule.getDepartment().getId())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy phòng ban với ID: " + schedule.getDepartment().getId()));

                UserInformationEntity createdBy = userRepository.findById(schedule.getCreatedBy())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người tạo với ID: " + schedule.getCreatedBy()));

                entity.setDepartment(department);

                UserInformationEntity createdBys = userRepository.findById(schedule.getCreatedBy())
                        .orElseThrow(() -> new Exceptions.ErrorException("Không tìm thấy người tạo với ID: " + schedule.getCreatedBy()));

                entity.setCreatedBy(createdBys.getId());

                consultationScheduleRepository.save(entity);
            } catch (Exception e) {
                throw new Exceptions.ErrorException("Lỗi khi lưu Consultation Schedule vào database: " + e.getMessage());
            }
        });
    }

    @Override
    public void importManageConsultantSchedules(List<List<String>> csvData) {
        // Skip header row
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


}

