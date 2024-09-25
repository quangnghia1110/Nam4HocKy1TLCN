package studentConsulting.service;

import java.time.LocalDate;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.consultation.ConsultationScheduleEntity;
import studentConsulting.model.payload.dto.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ConsultationFeedbackRequest;
import studentConsulting.model.payload.request.consultant.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;

public interface IConsultationScheduleService {
	ConsultationScheduleDTO createConsultation(CreateScheduleConsultationRequest request,
			UserInformationEntity user);

    public Page<ConsultationScheduleDTO> getSchedulesByUserWithFilters(UserInformationEntity user, Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);


    public Page<ConsultationScheduleDTO> getConsultationsByConsultantWithFilters(
            UserInformationEntity consultant, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,
            LocalDate startDate, LocalDate endDate, Pageable pageable);

	void confirmConsultationSchedule(Integer scheduleId, ConsultationFeedbackRequest request,
			UserInformationEntity consultant);
	
    public Optional<ConsultationScheduleEntity> findConsulationScheduleById(Integer scheduleId);

    public Page<ConsultationScheduleDTO> getConsultationsByDepartmentWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,LocalDate startDate, LocalDate endDate, Pageable pageable);

    public ManageConsultantScheduleDTO createConsultationSchedule(ManageCreateConsultantScheduleRequest request, Integer departmentId);

    public Page<ManageConsultantScheduleDTO> getConsultationsByDepartmentOwnerWithFilters(Integer departmentId, String title, Boolean statusPublic, Boolean statusConfirmed, Boolean mode,LocalDate startDate, LocalDate endDate, Pageable pageable);
    public ManageConsultantScheduleDTO updateConsultationSchedule(Integer scheduleId, Integer departmentId, UpdateConsultationScheduleRequest request);
    public void deleteConsultationSchedule(Integer scheduleId, Integer departmentId);
}

