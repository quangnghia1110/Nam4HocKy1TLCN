package studentConsulting.controller.common;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.model.payload.dto.DepartmentDTO;
import studentConsulting.model.payload.dto.FieldDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.common.ICommonDepartmentService;

import java.util.List;

@RestController
@RequestMapping("${base.url}")
public class CommonDepartmentController {

    @Autowired
    private ICommonDepartmentService departmentService;

    @GetMapping("/list-department")
    public ResponseEntity<DataResponse<List<DepartmentDTO>>> getAllDepartments() {
        List<DepartmentDTO> departments = departmentService.getAllDepartments();
        DataResponse<List<DepartmentDTO>> response = DataResponse.<List<DepartmentDTO>>builder()
                .status("success")
                .message("Fetched all departments successfully.")
                .data(departments)
                .build();

        return ResponseEntity.ok(response);
    }

    @GetMapping("/list-field-by-department")
    public ResponseEntity<DataResponse<List<FieldDTO>>> getFieldsByDepartment(@RequestParam Integer departmentId) {
        List<FieldDTO> fields = departmentService.getFieldsByDepartment(departmentId);
        DataResponse<List<FieldDTO>> response = DataResponse.<List<FieldDTO>>builder()
                .status("success")
                .message("Fetched fields for department ID: " + departmentId + " successfully.")
                .data(fields)
                .build();

        return ResponseEntity.ok(response);
    }
}
