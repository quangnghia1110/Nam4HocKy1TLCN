package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.*;

import java.time.LocalDate;
import java.util.Optional;

public interface IConsultationScheduleService {
    ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request,
                                               UserInformationEntity user);

    Page<ConsultationScheduleDTO> getSchedulesByUserWithFilters(UserInformationEntity user, Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);


    Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable);

    void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request,
                                     UserInformationEntity consultant);

    Optional<ConsultationScheduleEntity> findConsulationScheduleById(Integer scheduleId);

    Page<ConsultationScheduleDTO> getConsultationsByDepartmentWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId);

    Page<ManageConsultantScheduleDTO> getConsultationsByDepartmentOwnerWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode, LocalDate startDate, LocalDate endDate, Pageable pageable);

    ManageConsultantScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);

    void deleteConsultationSchedule(Integer scheduleId, Integer departmentId);

    ConsultationScheduleRegistrationDTO registerForConsultation(ConsultationScheduleRegistrationRequest request, UserInformationEntity user);


}

