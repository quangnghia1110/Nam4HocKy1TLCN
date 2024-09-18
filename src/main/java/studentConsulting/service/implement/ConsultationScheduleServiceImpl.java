package studentConsulting.service.implement;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.entity.departmentField.DepartmentEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.repository.ConsultationScheduleRepository;
import studentConsulting.repository.DepartmentRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IConsultationScheduleService;
import studentConsulting.specification.ConsultationScheduleSpecification;

@Service
public class ConsultationScheduleServiceImpl implements IConsultationScheduleService {

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private DepartmentRepository departmentRepository;

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
    public Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable) {

        Specification<ConsultationScheduleEntity> spec = Specification.where(ConsultationScheduleSpecification.hasConsultant(consultant));

        if (title != null) {
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

        return consultationScheduleRepository.findAll(spec, pageable).map(this::mapToDTO);
    }


    
    
    @Override
    public void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request, UserInformationEntity consultant) {
        List<FieldErrorDetail> errors = new ArrayList<>();

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new RuntimeException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
        	errors.add(new FieldErrorDetail("schedule","Lịch đã được xác nhận trước đó."));
        }

        if (request.getStatusConfirmed()) {
            if (schedule.getMode() != null && schedule.getMode()) { // Online
            	if (request.getLocation() != null) {
            		errors.add(new FieldErrorDetail("schedule","Không được phép nhập địa điểm cho tư vấn online."));
                }
            	if (request.getLink() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                	errors.add(new FieldErrorDetail("schedule","Phải cung cấp đầy đủ thông tin link, ngày và giờ cho tư vấn online."));
                }
                schedule.setLink(request.getLink());
            } else { // Offline
            	if (request.getLink() != null) {
            		errors.add(new FieldErrorDetail("schedule","Không được phép nhập link cho tư vấn offline."));
                }
            	if (request.getLocation() == null || request.getConsulationDate() == null || request.getConsultationTime() == null) {
                	errors.add(new FieldErrorDetail("schedule","Phải cung cấp đầy đủ thông tin địa điểm, ngày và giờ cho tư vấn offline."));
                }
                schedule.setLocation(request.getLocation());
            }
            schedule.setStatusConfirmed(true);
            schedule.setConsultationDate(request.getConsulationDate());
            schedule.setConsultationTime(request.getConsultationTime());
        } else {
            schedule.setStatusConfirmed(false);
        }
        
        if (!errors.isEmpty()) {
            throw new CustomFieldErrorException(errors);
        }

        consultationScheduleRepository.save(schedule);
    }
    
    @Override
    public Optional<ConsultationScheduleEntity> findConsulationScheduleById(Integer scheduleId) {
        return consultationScheduleRepository.findConsulationScheduleById(scheduleId);
    }
    
    private ConsultationScheduleDTO mapToDTO(ConsultationScheduleEntity schedule) {
        return ConsultationScheduleDTO.builder()
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
                .build();
    }
}
